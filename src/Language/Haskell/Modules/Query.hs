{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections, TypeFamilies #-}

-- | Ask questions about modules - unused imports etc.

module Language.Haskell.Modules.Query
    ( DeclaredSymbols(declaredSymbols)
    , ExportedSymbols(exportedSymbols')
    , ImportedModules(importedModules)
    , ImportedSymbols(importedSymbols)
    , ReferencedNames(referencedNames)
    , referencedSymbols
    , lookupNames
    , declares, exports, imports, uses
    ) where

import Debug.Trace (trace)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)
import Data.Set as Set (difference, empty, filter, fromList, Set, singleton, union, unions)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.Info
import Language.Haskell.Modules.Utils (gFind)
import Language.Haskell.Names (Scoped(..), Symbol(..), Environment)
import Language.Haskell.Names.Exports (exportedSymbols)
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName, Table)
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols, moduleTable)
import Language.Haskell.Names.SyntaxUtils

-- | This is a wrapper around 'getTopDeclSymbols'
class DeclaredSymbols a where
    declaredSymbols :: a -> Set Symbol

instance (Ord l, Data l) => DeclaredSymbols (Environment, ModuleInfo l) where
    declaredSymbols (env, i) = declaredSymbols (env, _module i)

instance (Ord l, Data l) => DeclaredSymbols (Environment, Module l) where
    declaredSymbols (env, m) = Set.unions (fmap (\d -> declaredSymbols (env, m, d)) (getModuleDecls m))

instance (Ord l, Data l) => DeclaredSymbols (Environment, Module l, Decl l) where
    declaredSymbols (env, m, d) = Set.fromList (getTopDeclSymbols (moduleGlobals env m) (getModuleName m) d)

-- | This duplicates the function of 'exportedSymbols'
class ExportedSymbols a where
    exportedSymbols' :: a -> Set Symbol

instance (Ord l, Data l) => ExportedSymbols (Environment, ModuleInfo l) where
    exportedSymbols' (env, i) = exportedSymbols' (env, _module i)

instance (Ord l, Data l) => ExportedSymbols (Environment, ModuleName l) where
    -- Find all the of the module's symbols in the global symbol table
    exportedSymbols' (env, name) =
        -- It might be worth returning a Maybe to preserve the fact
        -- that the module is not present in the environment.
        maybe Set.empty Set.fromList (Map.lookup (dropAnn name) env)

instance (Ord l, Data l) => ExportedSymbols (Environment, Module l) where
    exportedSymbols' (env, m) = Set.fromList (exportedSymbols (moduleTable (importTable env m) (dropAnn m)) m)
{-
        case getExportSpecList m of
          Nothing -> declaredSymbols (env, m)
          Just (ExportSpecList _ es) -> Set.unions (fmap (\e -> exportedSymbols' (env, m, e)) es)
-}
-- type Table = Map (QName ()) [Symbol]
-- type Environment = Map (ModuleName ()) [Symbol]

instance (Ord l, Data l) => ExportedSymbols (Environment, Module l, ExportSpec l) where
    exportedSymbols' (env, m, EVar _l qname) = Set.fromList (lookupName qname (moduleGlobals env m))
    exportedSymbols' (env, m, EThingWith _l _w qname _cname) = Set.fromList (lookupName qname (moduleGlobals env m))
    exportedSymbols' (env, m, EAbs _l _ns qname) = Set.fromList (lookupName qname (moduleGlobals env m))
    exportedSymbols' (env, m, EModuleContents _ name) =
        -- Find all symbols imported from name
        importedFrom (importedSymbols (env, m))
        where importedFrom :: Set Symbol -> Set Symbol
              importedFrom imports =
                  Set.filter (\sym -> symbolModule sym == dropAnn name) imports

class ImportedModules a where
    importedModules :: a -> Set (ModuleName ())

instance ImportedModules (ModuleName l) where
    importedModules = singleton . dropAnn

instance ImportedModules (Module l) where
    importedModules = Set.unions . fmap importedModules . getImports

instance ImportedModules (ImportDecl l) where
    importedModules = Set.singleton . dropAnn . importModule

-- | Symbols that are either imported or declared.  Note that
-- this duplicates the Language.Haskell.Names.Imports.importTable.
class ImportedSymbols a where
    importedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ImportedSymbols (Environment, ModuleInfo l) where
    importedSymbols (env, i) = importedSymbols (env, _module i)

instance (Ord l, Data l) => ImportedSymbols (Environment, Module l) where
    importedSymbols (env, m) = Set.unions (fmap (\i -> importedSymbols (env, m, i)) (getImports m))

instance (Ord l, Data l) => ImportedSymbols (Environment, Module l, ImportDecl l) where
    importedSymbols (env, m, ImportDecl {importModule = mname, importAs = aname, importSpecs = Just (ImportSpecList _ False isl)}) =
        Set.unions (fmap (\ispec -> importedSymbols (env, m, mname, aname, ispec)) isl)
    importedSymbols (env, m, ImportDecl {importModule = mname, importAs = aname, importSpecs = Just (ImportSpecList _ True isl)}) =
        -- All the symbols the module exports other than the ones in the list
        Set.difference (exportedSymbols' (env, mname)) (Set.unions (fmap (\i -> importedSymbols (env, m, mname, aname, i)) isl))
    importedSymbols (env, _m, ImportDecl {importModule = mname, importSpecs = Nothing}) =
        -- All the symbols in a module
        exportedSymbols' (env, mname)

instance ImportedSymbols (Environment, Module l, ModuleName l, Maybe (ModuleName l), ImportSpec l) where
    importedSymbols (env, m, mname, aname, IVar _l name) =
        Set.fromList (lookupName (toQName (fromMaybe mname aname) name) (moduleGlobals env m))
    importedSymbols (env, m, mname, aname, IAbs _l _space name) =
        Set.fromList (lookupName (toQName (fromMaybe mname aname) name) (moduleGlobals env m))
    importedSymbols (env, m, mname, aname, IThingAll _l name) =
        Set.fromList (lookupName (toQName (fromMaybe mname aname) name) (moduleGlobals env m))
    importedSymbols (env, m, mname, aname, IThingWith _l name _cnames) =
        Set.fromList (lookupName (toQName (fromMaybe mname aname) name) (moduleGlobals env m))

toQName :: ModuleName l -> Name l -> QName ()
toQName mname name = Qual () (dropAnn mname) (dropAnn name)

-- | Return the set of 'QName' used in an object such as a declaration.
-- One then builds a symbol table for the and looks these up 
-- For a module to be valid, each of these must either be imported or
-- declared.
class ReferencedNames a where
  referencedNames :: a -> Set (QName (Scoped SrcSpanInfo))

instance (l ~ Scoped SrcSpanInfo) => ReferencedNames (ModuleInfo l) where
    referencedNames i = referencedNames (_module i)

instance (l ~ Scoped SrcSpanInfo) => ReferencedNames (Module l) where
    referencedNames m =
        Set.unions (maybe Set.empty (\e -> referencedNames e) (getExportSpecList m) :
                    fmap (\d -> referencedNames d) (getModuleDecls m))

instance (l ~ Scoped SrcSpanInfo) => ReferencedNames (ExportSpecList l) where
    referencedNames x = Set.fromList names
        -- Set.fromList (concatMap (`lookupName` moduleGlobals env m) names)
        where names = fmap nameToQName (gFind x :: [Name l]) ++ gFind x :: [QName l]

instance (l ~ Scoped SrcSpanInfo) => ReferencedNames (Decl l) where
    -- The symbol being declared by a type signature does not count as
    -- a reference, if a symbol is declared in a module and has an
    -- accompanying signature it still may be "Defined but not used".
    referencedNames (TypeSig _ _names typ) = Set.fromList names
        -- ({-t1 x-} (Set.fromList (concatMap (`lookupName` moduleGlobals env m) names)))
        where names = fmap nameToQName (gFind ({-t1-} typ) :: [Name (Scoped SrcSpanInfo)]) ++ gFind typ :: [QName (Scoped SrcSpanInfo)]
    referencedNames x = Set.fromList names
{-
        (Set.difference
          ({-t1 x-} (Set.fromList (concatMap (`lookupName` moduleGlobals env m) names)))
          ({-t2 x-} (declaredSymbols (env, m, x))))
-}
        where names = fmap nameToQName (gFind x :: [Name (Scoped SrcSpanInfo)]) ++ gFind x :: [QName (Scoped SrcSpanInfo)]

-- t1 x = trace ("typesig type: " ++ show x) x
-- t2 d r = trace ("declared by " ++ show d ++ ": " ++ show r) r

lookupNames :: Set (QName (Scoped SrcSpanInfo)) -> Table -> Set Symbol
lookupNames names table = foldl (\r name -> Set.union r (Set.fromList (lookupName name table))) Set.empty names

referencedSymbols :: (l ~ Scoped SrcSpanInfo) => Environment -> Module l -> Set Symbol
referencedSymbols env m = lookupNames (referencedNames m) (moduleGlobals env m)

-- | Symbols declared by a declaration.  (Should take a single element, not a set.)
declares :: (Data l, Ord l) => Environment -> ModuleInfo l -> Decl l -> Set Symbol
declares env i d = declaredSymbols (env, _module i, d)

exports :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> ExportSpec (Scoped l) -> Set Symbol
exports env i espec = exportedSymbols' (env, _module i, espec)

imports :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> ModuleName (Scoped l) -> Maybe (ModuleName (Scoped l)) -> ImportSpec (Scoped l) -> Set Symbol
imports env i mname aname ispec = importedSymbols (env, _module i, mname, aname, ispec)

-- | Symbols used in a declaration - a superset of declares.
uses :: Table -> Decl (Scoped SrcSpanInfo) -> Set Symbol
uses table b = lookupNames (referencedNames b) table
