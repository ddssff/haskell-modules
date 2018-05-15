-- | Clean up the import list.

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Clean
    ( cleanImports
    ) where

import Debug.Trace
--import Control.Monad.State (State)
--import Data.Foldable as Foldable
--import Data.Generics
--import Data.Graph.Inductive (Gr, NodeMap)
import Data.Map as Map (Map, member)
import Data.Maybe -- (mapMaybe)
--import Data.Set as Set (difference, filter, empty, fromList, insert, intersection, map, member, null, Set, toList, union, unions)
--import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
--import Language.Haskell.Modules.FGL
--import Language.Haskell.Modules.Info
--import Language.Haskell.Modules.Query
--import Language.Haskell.Modules.Utils
import Language.Haskell.Names (Environment, Symbol)
--import Language.Haskell.Names.GlobalSymbolTable (Table)
import Language.Haskell.Names.Imports (importTable)
--import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils -- (dropAnn, getImports, getModuleDecls)

-- It doesn't work to just remove the elements from the module,
-- because the spans and the text remain the same.  We need to
-- build a predicate and pass it to render
cleanImports :: forall l. Show l => Environment -> Module l -> (ImportSpecWithDecl (Scoped l) -> Bool)
cleanImports env m@(Module l h ps is ds) =
  Module l h ps (fmap cleanImportDecl is) ds
  where
    table :: Map (QName ()) [Symbol]
    table = importTable env m
    cleanImportDecl :: ImportDecl l -> ImportDecl l
    cleanImportDecl i@(ImportDecl {importModule = mname,
                                   importAs = aname,
                                   importSpecs = Just (ImportSpecList l' False isl)}) =
        i {importSpecs = Just (ImportSpecList l' False (mapMaybe (cleanImportSpec mname aname . t2) []))}
    cleanImportDecl (ImportDecl {importModule = _mname, importAs = _aname, importSpecs = Just (ImportSpecList _ True _isl)}) = -- hiding
        undefined
    cleanImportDecl i@(ImportDecl {importModule = _mname, importSpecs = Nothing}) = undefined -- i

    cleanImportSpec :: ModuleName l -> Maybe (ModuleName l) -> ImportSpec l -> Maybe (ImportSpec l)
    cleanImportSpec _ _ _ = Nothing
    cleanImportSpec mname _aname i@(IVar _l name) =
        let qname = (Qual () (dropAnn mname) (dropAnn name)) in
        if t1 qname (Map.member qname table) then Just i else Nothing
    cleanImportSpec _mname _aname (IAbs _l _space _name) =
        error "cleanImportSpec"
    cleanImportSpec mname _aname i@(IThingAll _l name) =
        let qname = (Qual () (dropAnn mname) (dropAnn name)) in
        if t1 qname (Map.member qname table) then Just i else Nothing
    cleanImportSpec mname _aname i@(IThingWith _l name _cnames) =
        let qname = (Qual () (dropAnn mname) (dropAnn name)) in
        if t1 qname (Map.member qname table) then Just i else Nothing
cleanImports _env m = m

t1 qname x = trace ("member " ++ show qname ++ ": " ++ show x) x
t3 x = {-trace ("\nidecl: " ++ show (dropAnn x))-} x
t2 x = trace ("\nispec: " ++ show x) x
{-
    (Set.difference
                     (importedSymbols (env', i))
                     (Set.union
                        (referencedSymbols (env', i))
                        (exportedSymbols (env', i))))
-}
