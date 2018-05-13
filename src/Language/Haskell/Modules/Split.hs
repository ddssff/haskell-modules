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
import Language.Haskell.Modules.Utils
import Language.Haskell.Names (Scoped(..), Symbol)
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName)
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

declGroupName :: (Data l, Eq l) => ModuleInfo l -> DeclGroup l -> Symbol
declGroupName i (DeclGroup ds) =
    case Set.toList (foldr1 Set.intersection (fmap (declares i) ds)) of
      [s] -> s
      [] -> error "declGroupName 1"
      _ -> error "declGroupName 2"

withDecomposedModule ::
    forall r l. (l ~ SrcSpanInfo)
    => (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) [[DeclGroup (Scoped l)]])
    -> (ModuleInfo (Scoped SrcSpanInfo)
        -> (Decl (Scoped SrcSpanInfo) -> Bool)
        -> (ExportSpec (Scoped SrcSpanInfo) -> Bool)
        -> (ImportSpecWithDecl (Scoped SrcSpanInfo) -> Bool)
        -> r)
    -> ModuleInfo (Scoped SrcSpanInfo) -> [r]
withDecomposedModule decompose f i@(ModuleInfo {_module = Module _l _h _ps _is _ds, _moduleComments = _cs}) =
    fmap (uncurry3 (f i)) (zip3 (fmap (flip Set.member) selectedDecls)
                                (fmap (flip Set.member) selectedExports)
                                (fmap (flip Set.member) selectedImports))
    where
      selectedDecls :: [Set (Decl (Scoped SrcSpanInfo))]
      selectedDecls = partitionDeclsBy decompose i
      selectedExports:: [Set (ExportSpec (Scoped SrcSpanInfo))]
      selectedExports = fmap (exportsToKeep i) selectedDecls
      selectedImports :: [Set (ImportSpecWithDecl (Scoped SrcSpanInfo))]
      selectedImports = fmap (uncurry (importsToKeep i)) (zip selectedDecls selectedExports)
{-
      selectedComments = fmap (uncurry (filterComments (fmap (srcInfoSpan . unScope) i)))
                           (zip (fmap (Set.map (fmap (srcInfoSpan . unScope))) selectedDecls)
                                (fmap (Set.map (fmap (srcInfoSpan . unScope))) selectedExports))
-}
withDecomposedModule _decompose f i@(ModuleInfo {_module = _m}) =
    [f i (const True) (const True) (const True)]

-- | Partition a module's declarations according to the graph of connected components
-- in the "declares - uses" graph.
partitionDeclsBy ::
    forall l. (Data l, Eq l, Ord l, Show l)
    => (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) [[DeclGroup (Scoped l)]])
    -- ^ A query on the "uses" graph, partitions the declaration groups.
    -> ModuleInfo (Scoped l)
    -> [Set (Decl (Scoped l))]
partitionDeclsBy decompose i = do
  fst $ withUsesGraph i $ \g -> do
    (tmp :: [[DeclGroup (Scoped l)]]) <- decompose g
    return $ fmap (Set.fromList . concat . fmap unDecs) tmp

-- | Build a graph whose nodes are declaration groups and whose edges
-- are the "declares, uses" relation.  Each edge is labeled with a set
-- of symbols.
withUsesGraph ::
    forall l r. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) r)
    -> (r, NodeMap (DeclGroup (Scoped l)))
withUsesGraph i f =
    runGraph (mkGraphM declGroups (concatMap (\a -> mapMaybe (edge a) declGroups) declGroups) >>= f)
    where
      declGroups :: [DeclGroup (Scoped l)]
      declGroups = groupDecs i (getModuleDecls (_module i))
      -- Create edges from any declaration A to any other declaration
      -- B such that A declares a symbol that B uses.
      edge :: DeclGroup (Scoped l) -> DeclGroup (Scoped l) -> Maybe (DeclGroup (Scoped l), DeclGroup (Scoped l), Set Symbol)
      edge a b = if Set.null common then Nothing else Just (a, b, common)
          where common = Set.intersection (Set.unions (fmap (declares i) (unDecs a))) (uses i b)

-- | Declarations come in sets - a signature, followed by one or more
-- Decls.
groupDecs :: forall l. (Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> [Decl (Scoped l)] -> [DeclGroup (Scoped l)]
groupDecs _ [] = error "makeDecs - invalid argument"
groupDecs i (d1 : ds1) =
    -- With foldr we encounter the declarations in reverse, so the
    -- signature ends up with the previous declaration.
    snd $ foldl go (getTopDeclSymbols' i d1, [DeclGroup [d1]]) ds1
    where
      go :: (Set Symbol, [DeclGroup (Scoped l)]) -> Decl (Scoped l) -> (Set Symbol, [DeclGroup (Scoped l)])
      go (_, []) _ = (Set.empty, [])
      go (_ss, DeclGroup ds : more) d@(TypeSig {}) =
        -- Assuming signature comes first - is this a bad assumption?
          (mempty, DeclGroup [d] : DeclGroup ds : more)
      -- If the symbol set is empty we have seen a signature, this must(?)
      -- be the corresponding declaration.
      go (ss, DeclGroup ds : more) d | Set.null ss =
          let ss' = getTopDeclSymbols' i d in
          (Set.union ss ss', DeclGroup (ds ++ [d]) : more)
      go (ss, DeclGroup ds : more) d =
          let ss' = getTopDeclSymbols' i d in
          if Set.null (Set.intersection ss ss')
          then (ss', DeclGroup [d] : DeclGroup ds : more)
          else (Set.union ss ss', DeclGroup (ds ++ [d]) : more)

-- | Return the set of ExportSpec that are not supplied by the set of
-- declarations.
exportsToKeep ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
exportsToKeep i@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _(Just (ExportSpecList _ es)))) _ _ _}) ds  =
  Foldable.foldl go Set.empty es
  where
    syms = foldr1 Set.union (Set.map (declares i) ds)
    go r e = if not (Set.null (Set.intersection syms (exports i e))) then Set.insert e r else r
exportsToKeep _ _ = Set.empty

-- | Result is a set of pairs, an ImportDecl and some ImportSpec that
-- could be in its ImportSpecList.
importsToKeep ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
    -> Set (ImportSpecWithDecl (Scoped l))
importsToKeep i@(ModuleInfo {_module = Module _ _ _ is _}) ds es =
  foldl goDecl Set.empty is
  where
    -- We need to keep any import if it is either used or re-exported
    syms = Set.union
             (uses i ds)
             (flatten (Set.map (exports i) es))
             -- (declares i ds) -- All the symbols declared in this module
             -- (error "importsToKeep" :: Set Symbol)
             -- (Set.unions (fmap (exports i) (es :: [ExportSpec (Scoped l)]) :: [Set Symbol]))  -- All the symbols exported by this module
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
        if not (Set.null (Set.intersection (imports i ispec) syms))
        then Set.insert (idecl, ispec) r
        else r
importsToKeep _ _ _ = error "importsToKeep"

{-
-- | We now have sets describing which declarations to keep and which
-- export specs to keep.  This function partitions the comment list.
-- (Comments associated with the header outside the export list are
-- duplicated in all modules.)
filterComments ::
       ModuleInfo SrcSpan
    -> Set (Decl SrcSpan)
    -> Set (ExportSpec SrcSpan)
    -> Set Comment
filterComments (ModuleInfo {_module = Module _l h _ps _is ds, _moduleComments = cs}) selectedDecls selectedExports =
    -- In the export list we remove anything before a removed
    -- ExportSpec, and if the final ExportSpec is removed we remove
    -- anything after it until the end of the ExportSpecList.
    Set.fromList (prescan1 exportStatus cs [])
    --Set.union (selectedComments cs Set.empty) filterDeclComments
    where
      exportStatus :: [(ExportSpec SrcSpan, Bool)]
      exportStatus = case h of
                       Just (ModuleHead _ _ _ (Just (ExportSpecList _ es))) -> zip es (fmap (`Set.member` selectedExports) es)
                       _ -> []
      declStatus :: [(Decl SrcSpan, Bool)]
      declStatus = zip ds (fmap (`Set.member` selectedDecls) ds)

      prescan1 :: [(ExportSpec SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      prescan1 es@((e, _) : _more) (c1 : c2@(Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanStart (ann e) = prescan1 es (c2 : cs') (c1 : r)
      prescan1 es cs' r = scanExports es cs' r

      scanExports :: [(ExportSpec SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      scanExports es@((e, False) : _more) ((Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanEnd (ann e) = scanExports es cs' r
      scanExports es@((e, True) : _more) (c1@(Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanEnd (ann e) = scanExports es cs' (c1 : r)
      scanExports ((_e, _) : more) (c1 : cs') r = scanExports more (c1 : cs') r
      scanExports [] cs' r = prescan2 declStatus cs' r
      scanExports _ [] r = prescan2 declStatus cs r

      -- Scan comments thru the one before the one before the first decl
      prescan2 :: [(Decl SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      prescan2 ds'@((d, _) : _more) (c1 : c2@(Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanStart (ann d) = prescan2 ds' (c2 : cs') (c1 : r)
      prescan2 ds' cs' r = scanDecls ds' cs' r
      -- We have reached the comment that directly precedes the
      -- declaration section.
      scanDecls :: [(Decl SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      scanDecls ds'@((d, False) : _more) ((Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanEnd (ann d) = scanDecls ds' cs' r
      scanDecls ds'@((d, True) : _more) (c1@(Comment _ cspan _) : cs') r
        | srcSpanEnd cspan < srcSpanEnd (ann d) = scanDecls ds' cs' (c1 : r)
      scanDecls ((_,  _) : more) (c1 : cs') r = scanDecls more (c1 : cs') r
      scanDecls [] cs' r = reverse r ++ cs'
      scanDecls _ [] r = reverse r
filterComments _ _ _ = error "filterComments"
-}

-- | Symbols declared by a declaration.  (Should take a single element, not a set.)
declares :: (Data l, Eq l) => ModuleInfo l -> Decl l -> Set Symbol
declares i ds = getTopDeclSymbols' i ds

exports :: forall l. (Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> ExportSpec (Scoped l) -> Set Symbol
exports i (EVar _l qname) = Set.fromList (lookupName qname (_moduleGlobals i))
exports i (EThingWith _l _w qname _cname) = Set.fromList (lookupName qname (_moduleGlobals i))
exports i (EAbs _l _ns qname) = Set.fromList (lookupName qname (_moduleGlobals i))
exports _i (EModuleContents _ (ModuleName _l _string)) =
    -- Re-exports all the symbols that were imported from a module
    Set.empty

imports :: forall l. (Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> ImportSpec (Scoped l) -> Set Symbol
imports i (IVar _l name) = Set.fromList (lookupName (nameToQName name) (_moduleGlobals i))
imports i (IAbs _l _space name) = Set.fromList (lookupName (nameToQName name) (_moduleGlobals i))
imports i (IThingAll _l name) = Set.fromList (lookupName (nameToQName name) (_moduleGlobals i))
imports i (IThingWith _l name _cnames) = Set.fromList (lookupName (nameToQName name) (_moduleGlobals i))

-- | Symbols used in a declaration - a superset of declares.
uses :: forall l d. (Data d, Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> d -> Set Symbol
uses i b = Set.fromList (concatMap (`lookupName` (_moduleGlobals i)) names)
    where
      names :: [QName (Scoped l)]
      names = fmap nameToQName (gFind b :: [Name (Scoped l)]) ++ gFind b :: [QName (Scoped l)]

getTopDeclSymbols' :: (Data l, Eq l) => ModuleInfo l -> Decl l -> Set Symbol
getTopDeclSymbols' i d = Set.fromList $ getTopDeclSymbols (_moduleGlobals i) (getModuleName (_module i)) d
