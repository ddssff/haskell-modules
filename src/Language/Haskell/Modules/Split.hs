-- | Split a modle in several ways based on the structure of the
-- "declares -> uses" relation on top level declarations.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Split
    ( bisect
    , withDecomposedModule
    , withUsesGraph
    , cleanImports
    ) where

import Debug.Trace ( trace )
import Control.Monad.State ( State )
import Data.Foldable as Foldable ( Foldable(foldl) )
import Data.Generics ( Data )
import Data.Graph.Inductive ( Gr, NodeMap )
import Data.List ( intercalate )
import Data.Map as Map ( keys, lookup, Map )
import Data.Maybe ( fromMaybe )
import Data.Set as Set
    ( difference, filter, empty, fromList, insert, intersection, map, member, null, Set, toList, union, unions )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import Language.Haskell.Exts.Syntax
    ( QName(Qual), Name, ModuleHead(ModuleHead), Module(Module), ImportSpecList(ImportSpecList), ImportSpec(..),
      ImportDecl(ImportDecl, importAs, importModule, importSpecs), ExportSpecList(ExportSpecList), ExportSpec, Decl )
import Language.Haskell.Modules.FGL ( labNodes, reachable )
import Language.Haskell.Modules.Graphs ( DeclGroup(unDecs), withUsesGraph )
import Language.Haskell.Modules.Info ( ImportSpecWithDecl, ModuleInfo(ModuleInfo, _moduleComments, _module), moduleGlobals )
import Language.Haskell.Modules.Query ( ReferencedNames(referencedNames), lookupNames, declares, exports, imports, uses )
import Language.Haskell.Modules.Utils ( flatten, uncurry3 )
import Language.Haskell.Names ( Environment, Scoped(..), Symbol )
import Language.Haskell.Names.Imports ( importTable )
import Language.Haskell.Names.SyntaxUtils ( dropAnn )

-- | Split a graph into two components, those reachable from any of the
-- nodes selected by the predicate, and the rest.  This is what you use
-- to pull a function out of a module.  The resulting module will usually
-- need imports from the old, and presumably an export should be added for
-- the function that was extracted.
bisect ::
    (Ord a, Show a)
    => (a -> Bool)
    -> Gr a b
    -> State (NodeMap a) [[a]]
bisect p g = do
  theseNodes <- mapM (reachable g) (Set.toList pNodes)
  let theseNodes' = Set.unions theseNodes
  let thoseNodes = Set.difference allNodes theseNodes'
  return [Set.toList theseNodes', Set.toList thoseNodes]
    where
      allNodes = Set.fromList (labNodes g)
      pNodes = Set.filter p allNodes

withDecomposedModule ::
    forall r l. (l ~ SrcSpanInfo)
    => Environment
    -> (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) [[DeclGroup (Scoped l)]])
    -> (ModuleInfo (Scoped SrcSpanInfo)
        -> (Decl (Scoped SrcSpanInfo) -> Bool)
        -> (ExportSpec (Scoped SrcSpanInfo) -> Bool)
        -> (ImportSpecWithDecl (Scoped SrcSpanInfo) -> Bool)
        -> r)
    -> ModuleInfo (Scoped SrcSpanInfo) -> [r]
withDecomposedModule env decompose f i@(ModuleInfo {_module = Module _l _h _ps _is _ds, _moduleComments = _cs}) =
    fmap (uncurry3 (f i)) (zip3 (fmap (flip Set.member) selectedDecls)
                                (fmap (flip Set.member) selectedExports)
                                (fmap (flip Set.member) selectedImports))
    where
      selectedDecls :: [Set (Decl (Scoped SrcSpanInfo))]
      selectedDecls = partitionDeclsBy env decompose i
      selectedExports :: [Set (ExportSpec (Scoped SrcSpanInfo))]
      selectedExports = fmap (exportsToKeep env i) selectedDecls
      selectedImports :: [Set (ImportSpecWithDecl (Scoped SrcSpanInfo))]
      selectedImports = fmap (uncurry (importsToKeep env i)) (zip selectedDecls selectedExports)
withDecomposedModule _env _decompose f i@(ModuleInfo {_module = _m}) =
    [f i (const True) (const True) (const True)]

-- | Partition a module's declarations according to the graph of connected components
-- in the "declares - uses" graph.
partitionDeclsBy ::
    forall l. (l ~ SrcSpanInfo)
    => Environment
    -> (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) [[DeclGroup (Scoped l)]])
    -- ^ A query on the "uses" graph, partitions the declaration groups.
    -> ModuleInfo (Scoped l)
    -> [Set (Decl (Scoped l))]
partitionDeclsBy env decompose i = do
  fst $ withUsesGraph env i $ \g -> do
    (tmp :: [[DeclGroup (Scoped l)]]) <- decompose g
    return $ fmap (Set.fromList . concat . fmap unDecs) tmp

-- | Return the set of ExportSpec that are not supplied by the set of
-- declarations.
exportsToKeep ::
    forall l. (Data l, Ord l, Show l)
    => Environment
    -> ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
exportsToKeep env i@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ es)))) _ _ _}) ds  =
  Foldable.foldl go Set.empty es
  where
    syms = foldr1 Set.union (Set.map (declares env i) ds)
    go r e = if not (Set.null (Set.intersection syms (exports env i e))) then Set.insert e r else r
exportsToKeep _ _ _ = Set.empty

-- | Result is a set of pairs, an ImportDecl and some ImportSpec that
-- could be in its ImportSpecList.
importsToKeep ::
    forall l. (l ~ SrcSpanInfo)
    => Environment
    -> ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
    -> Set (ImportSpecWithDecl (Scoped l))
importsToKeep env i@(ModuleInfo {_module = Module _ _ _ is _}) ds es =
  foldl goDecl Set.empty is
  where
    -- We need to keep any import if it is either used or re-exported
    syms = Set.union
             (flatten (Set.map (uses (moduleGlobals env (_module i))) ds))
             (flatten (Set.map (exports env i) es))
             -- (declares i ds) -- All the symbols declared in this module
             -- (error "importsToKeep" :: Set Symbol)
             -- (Set.unions (fmap (exports env i) (es :: [ExportSpec (Scoped l)]) :: [Set Symbol]))  -- All the symbols exported by this module
    -- Keep any imports of symbols that are declared or exported
    goDecl :: Set (ImportSpecWithDecl (Scoped l)) -> ImportDecl (Scoped l) -> Set (ImportSpecWithDecl (Scoped l))
    goDecl r idecl@(ImportDecl {importSpecs = Just (ImportSpecList _ False isl)}) = foldl (goSpec idecl) r isl
    goDecl r (ImportDecl {importSpecs = Just (ImportSpecList _ True _isl)}) =
        -- This is a hiding declaration, need to think about what to do
        r
    goDecl r (ImportDecl {importSpecs = Nothing}) = r
    goSpec ::
           ImportDecl (Scoped l)
        -> Set (ImportSpecWithDecl (Scoped l))
        -> ImportSpec (Scoped l)
        -> Set (ImportSpecWithDecl (Scoped l))
    goSpec idecl r ispec =
        if not (Set.null (Set.intersection (imports env i (importModule idecl) (importAs idecl) ispec) syms))
        then Set.insert (idecl, ispec) r
        else r
importsToKeep _ _ _ _ = error "importsToKeep"

cleanImports ::
    forall l. (l ~ Scoped SrcSpanInfo)
    => Environment
    -> ModuleInfo l
    -> ImportSpecWithDecl l
    -> Bool
cleanImports env i@(ModuleInfo {_module = m@(Module _ _ _ is _)}) =
    flip Set.member (foldl goDecl Set.empty is)
    where
      refs :: Set Symbol
      refs = lookupNames (referencedNames m) (moduleGlobals env m)
      -- I *think* the qnames here are the same as the ones I compute
      -- below for each import spec.  Any import specs whose qname
      -- maps to symbols that are not in the set refs should be
      -- dropped.
      table :: Map (QName ()) [Symbol]
      table = importTable env (_module i)
      goDecl :: Set (ImportSpecWithDecl l) -> ImportDecl l -> Set (ImportSpecWithDecl l)
      goDecl r idecl@(ImportDecl {importModule = mname,
                                  importAs = aname,
                                  importSpecs = Just (ImportSpecList _ False isl)}) = foldl (\r s -> goSpec idecl (\name -> Qual () (dropAnn (fromMaybe mname aname)) (dropAnn name)) r (t5 s)) r isl
      goDecl r (ImportDecl {importSpecs = Just (ImportSpecList _ _hiding@True _isl)}) = r
      goDecl r (ImportDecl {importSpecs = Nothing}) = r
      goSpec ::
           ImportDecl l
        -> (Name l -> QName ())
        -> Set (ImportSpecWithDecl l)
        -> ImportSpec l
        -> Set (ImportSpecWithDecl l)
      goSpec idecl qname r ispec@(IVar _ name) =
          case Map.lookup (qname name) table of
            Nothing -> error $ "Wrong qname: " ++ show (qname name) ++ " (table1: " ++ show (Map.keys table) ++ ")"
            Just syms -> case Prelude.filter (`Set.member` refs) (t3 (qname name) syms) of
                           [] -> t6 (qname name) refs r
                           _ -> Set.insert (idecl, ispec) r
      goSpec idecl qname r ispec@(IAbs _l _space name) =
          case Map.lookup (qname name) table of
            Nothing -> error $ "Wrong qname: " ++ show (qname name) ++ " (table2: " ++ show (Map.keys table) ++ ")"
            Just syms -> case Prelude.filter (`Set.member` refs) (t4 (qname name) syms) of
                           [] -> t6 (qname name) refs r
                           _ -> Set.insert (idecl, ispec) r
      goSpec idecl qname r ispec@(IThingAll _l name) =
          case Map.lookup (qname name) table of
            Nothing -> error $ "Wrong qname: " ++ show (qname name) ++ " (table3: " ++ show (Map.keys table) ++ ")"
            Just syms -> case Prelude.filter (`Set.member` refs) (t2 (qname name) syms) of
                           [] -> t6 (qname name) refs r
                           _ -> Set.insert (idecl, ispec) r
      goSpec idecl qname r ispec@(IThingWith _l name _cnames) =
          case Map.lookup (qname name) table of
            Nothing -> error $ "Wrong qname: " ++ show (qname name) ++ " (table4: " ++ show (Map.keys table) ++ ")"
            Just syms -> case Prelude.filter (`Set.member` refs) (t1 (qname name) syms) of
                           [] -> t6 (qname name) refs r
                           _ -> Set.insert (idecl, ispec) r
        -- if not (Set.null (Set.intersection (imports env i (importModule idecl) (importAs idecl) ispec) syms))
        -- then Set.insert (idecl, ispec) r
        -- else r
cleanImports _ _ = const True

t1 n x = trace ("\nIThing\n qname: " ++ show n ++ "\n  syms: " ++ show x) x
t2 n x = trace ("\nIThingAll\n qname: " ++ show n ++ "\n  syms: " ++ show x) x
t3 n x = trace ("\nIVar\n qname: " ++ show n ++ "\n  syms: " ++ show x) x
t4 n x = trace ("\nIAbs\n qname: " ++ show n ++ "\n  syms: " ++ show x) x
t5 x = {-trace ("\nispec: " ++ show x)-} x
t6 n rs x = trace ("\nt6 " ++ show n ++ "\n refs:\n" ++ intercalate "\n    " (fmap show (Set.toList rs))) x
