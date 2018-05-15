-- | Split a module in several ways based on the structure of the
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
    , DeclGroup(unDecs)
    , declares
    , declGroupName
    ) where

import Control.Monad.State (State)
import Data.Foldable as Foldable
import Data.Generics
import Data.Graph.Inductive (Gr, NodeMap)
import Data.Maybe (mapMaybe)
import Data.Set as Set (difference, filter, empty, fromList, insert, intersection, map, member, null, Set, toList, union, unions)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.FGL
import Language.Haskell.Modules.Info
import Language.Haskell.Modules.Query
import Language.Haskell.Modules.Utils
import Language.Haskell.Names (Environment, Scoped(..), Symbol)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils -- (dropAnn, getImports, getModuleDecls)

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

-- | A group of declarations that should not be split up - e.g. a
-- signature and the corresponding declaration(s).
newtype DeclGroup l = DeclGroup {unDecs :: [Decl l]} deriving (Data, Eq, Ord, Show, Functor)

declGroupName :: (Data l, Ord l) => Environment -> ModuleInfo l -> DeclGroup l -> Symbol
declGroupName env i (DeclGroup ds) =
    case Set.toList (foldr1 Set.intersection (fmap (declares env i) ds)) of
      [s] -> s
      [] -> error "declGroupName 1"
      _ -> error "declGroupName 2"

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

-- | Build a graph whose nodes are declaration groups and whose edges
-- are the "declares, uses" relation.  Each edge is labeled with a set
-- of symbols.
withUsesGraph ::
    forall l r. (l ~ SrcSpanInfo)
    => Environment
    -> ModuleInfo (Scoped l)
    -> (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) r)
    -> (r, NodeMap (DeclGroup (Scoped l)))
withUsesGraph env i f =
    runGraph (mkGraphM declGroups (concatMap (\a -> mapMaybe (edge a) declGroups) declGroups) >>= f)
    where
      declGroups :: [DeclGroup (Scoped l)]
      declGroups = groupDecs env i (getModuleDecls (_module i))
      -- Create edges from any declaration A to any other declaration
      -- B such that A declares a symbol that B uses.
      edge :: DeclGroup (Scoped l) -> DeclGroup (Scoped l) -> Maybe (DeclGroup (Scoped l), DeclGroup (Scoped l), Set Symbol)
      edge a b = if Set.null common then Nothing else Just (a, b, common)
          where common = Set.intersection
                           (Set.unions (fmap (declares env i) (unDecs a)))
                           (Set.unions (fmap (uses env i) (unDecs b)))

-- | Declarations come in sets - a signature, followed by one or more
-- Decls.
groupDecs :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> [Decl (Scoped l)] -> [DeclGroup (Scoped l)]
groupDecs _ _ [] = error "makeDecs - invalid argument"
groupDecs env i (d1 : ds1) =
    -- With foldr we encounter the declarations in reverse, so the
    -- signature ends up with the previous declaration.
    snd $ foldl go (getTopDeclSymbols' env i d1, [DeclGroup [d1]]) ds1
    where
      go :: (Set Symbol, [DeclGroup (Scoped l)]) -> Decl (Scoped l) -> (Set Symbol, [DeclGroup (Scoped l)])
      go (_, []) _ = (Set.empty, [])
      go (_ss, DeclGroup ds : more) d@(TypeSig {}) =
        -- Assuming signature comes first - is this a bad assumption?
          (mempty, DeclGroup [d] : DeclGroup ds : more)
      -- If the symbol set is empty we have seen a signature, this must(?)
      -- be the corresponding declaration.
      go (ss, DeclGroup ds : more) d | Set.null ss =
          let ss' = getTopDeclSymbols' env i d in
          (Set.union ss ss', DeclGroup (ds ++ [d]) : more)
      go (ss, DeclGroup ds : more) d =
          let ss' = getTopDeclSymbols' env i d in
          if Set.null (Set.intersection ss ss')
          then (ss', DeclGroup [d] : DeclGroup ds : more)
          else (Set.union ss ss', DeclGroup (ds ++ [d]) : more)

-- | Return the set of ExportSpec that are not supplied by the set of
-- declarations.
exportsToKeep ::
    forall l. (Data l, Ord l, Show l)
    => Environment
    -> ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
exportsToKeep env i@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _(Just (ExportSpecList _ es)))) _ _ _}) ds  =
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
             (flatten (Set.map (uses env i) ds))
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

-- | Symbols declared by a declaration.  (Should take a single element, not a set.)
declares :: (Data l, Ord l) => Environment -> ModuleInfo l -> Decl l -> Set Symbol
declares env i d = declaredSymbols (env, _module i, d)

exports :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> ExportSpec (Scoped l) -> Set Symbol
exports env i espec = exportedSymbols (env, _module i, espec)

imports :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> ModuleName (Scoped l) -> Maybe (ModuleName (Scoped l)) -> ImportSpec (Scoped l) -> Set Symbol
imports env i mname aname ispec = importedSymbols (env, _module i, mname, aname, ispec)

-- | Symbols used in a declaration - a superset of declares.
uses :: Environment -> ModuleInfo (Scoped SrcSpanInfo) -> Decl (Scoped SrcSpanInfo) -> Set Symbol
uses env i b = referencedSymbols (env, _module i, b)

getTopDeclSymbols' :: (Data l, Eq l) => Environment -> ModuleInfo l -> Decl l -> Set Symbol
getTopDeclSymbols' env i d = Set.fromList $ getTopDeclSymbols (moduleGlobals env (_module i)) (getModuleName (_module i)) d
