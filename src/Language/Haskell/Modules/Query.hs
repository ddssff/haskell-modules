{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}

-- | Ask questions about modules - unused imports etc.

module Language.Haskell.Modules.Query
    ( DeclaredSymbols(declaredSymbols)
    , ExportedSymbols(exportedSymbols)
    , ImportedModules(importedModules)
    , ImportedSymbols(importedSymbols)
    ) where

import Data.Data (Data)
import Data.Map as Map (lookup)
import Data.Set as Set (difference, empty, filter, fromList, Set, singleton, unions)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.Info
import Language.Haskell.Names (Symbol(..), Environment)
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName, Table)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils

class DeclaredSymbols a where
    declaredSymbols :: a -> Set Symbol

instance (Ord l, Data l) => DeclaredSymbols (Environment, ModuleInfo l) where
    declaredSymbols (env, i) = declaredSymbols (env, _moduleGlobals i, _module i)

instance (Ord l, Data l) => DeclaredSymbols (Environment, Global.Table, Module l) where
    declaredSymbols (env, table, m) = Set.unions (fmap (\d -> declaredSymbols (env, table, getModuleName m, d)) (getModuleDecls m))

instance (Ord l, Data l) => DeclaredSymbols (Environment, Global.Table, ModuleName l, Decl l) where
    declaredSymbols (_env, table, mname, d) = Set.fromList (getTopDeclSymbols table mname d)

class ExportedSymbols a where
    exportedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ExportedSymbols (Environment, ModuleInfo l) where
    exportedSymbols (env, i) = exportedSymbols (env, _moduleGlobals i, _module i)

instance (Ord l, Data l) => ExportedSymbols (Environment, Global.Table, Module l) where
    exportedSymbols (env, table, m) =
        case getExportSpecList m of
          Nothing -> declaredSymbols (env, table, m)
          Just (ExportSpecList _ es) -> Set.unions (fmap (\e -> exportedSymbols (env, table, m, e)) es)

-- type Table = Map (QName ()) [Symbol]
-- type Environment = Map (ModuleName ()) [Symbol]

instance (Ord l, Data l) => ExportedSymbols (Environment, ModuleName l) where
    -- Find all the of the module's symbols in the global symbol table
    exportedSymbols (env, name) =
        -- It might be worth returning a Maybe to preserve the fact
        -- that the module is not present in the environment.
        maybe Set.empty Set.fromList (Map.lookup (dropAnn name) env)

instance (Ord l, Data l) => ExportedSymbols (Environment, Global.Table, Module l, ExportSpec l) where
    exportedSymbols (_env, table, _m, EVar _l qname) = Set.fromList (lookupName qname table)
    exportedSymbols (_env, table, _m, EThingWith _l _w qname _cname) = Set.fromList (lookupName qname table)
    exportedSymbols (_env, table, _m, EAbs _l _ns qname) = Set.fromList (lookupName qname table)
    exportedSymbols (env, table, m, EModuleContents _ name) =
        -- Find all symbols imported from name
        importedFrom (importedSymbols (env, table, m))
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

class ImportedSymbols a where
    importedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ImportedSymbols (Environment, ModuleInfo l) where
    importedSymbols (env, i) = importedSymbols (env, _moduleGlobals i, _module i)

instance (Ord l, Data l) => ImportedSymbols (Environment, Global.Table, Module l) where
    importedSymbols (env, table, m) = Set.unions (fmap (\i -> importedSymbols (env, table, i)) (getImports m))

instance (Ord l, Data l) => ImportedSymbols (Environment, Global.Table, ImportDecl l) where
    importedSymbols (env, table, ImportDecl {importModule = _mname, importSpecs = Just (ImportSpecList _ False isl)}) =
        Set.unions (fmap (\ispec -> importedSymbols (env, table, ispec)) isl)
    importedSymbols (env, table, ImportDecl {importModule = mname, importSpecs = Just (ImportSpecList _ True isl)}) =
        -- All the symbols the module exports other than the ones in the list
        Set.difference (exportedSymbols (env, mname)) (Set.unions (fmap (\i -> importedSymbols (env, table, i)) isl))
    importedSymbols (env, _table, ImportDecl {importModule = mname, importSpecs = Nothing}) =
        -- All the symbols in a module
        exportedSymbols (env, mname)

instance ImportedSymbols (Environment, Global.Table, ImportSpec l) where
    importedSymbols (_env, table, IVar _l name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (_env, table, IAbs _l _space name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (_env, table, IThingAll _l name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (_env, table, IThingWith _l name _cnames) = Set.fromList (lookupName (nameToQName name) table)

-- | Build the 'declares -> uses' relation on the top level symbols
-- of a module, then compute nodes that are not reachable from exported
-- symbols.
{-
unreferencedSymbols :: Environment -> Module (Scoped SrcSpanInfo) -> Set Symbol
unreferencedSymbols env modul =
  undefined
  where
    uses :: 
    exported = mapMaybe (undefined :: (getExportSpecList modul) :: Maybe (ExportSpecList l)
-}
