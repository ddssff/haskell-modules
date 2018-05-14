{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}

-- | Ask questions about modules - unused imports etc.

module Language.Haskell.Modules.Query
    ( DeclaredSymbols(declaredSymbols)
    , ExportedSymbols(exportedSymbols)
    , ImportedSymbols(importedSymbols)
    ) where

import Data.Data (Data)
import Data.Maybe (mapMaybe)
import Data.Set as Set (empty, filter, fromList, Set, unions)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.Info
import Language.Haskell.Names (Scoped(..), Symbol(..))
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName, Table)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils

class DeclaredSymbols a where
    declaredSymbols :: a -> Set Symbol

instance (Ord l, Data l) => DeclaredSymbols (ModuleInfo l) where
    declaredSymbols i = declaredSymbols (_moduleGlobals i, _module i)

instance (Ord l, Data l) => DeclaredSymbols (Global.Table, Module l) where
    declaredSymbols (table, m) = Set.unions (fmap (\d -> declaredSymbols (table, getModuleName m, d)) (getModuleDecls m))

instance (Ord l, Data l) => DeclaredSymbols (Global.Table, ModuleName l, Decl l) where
    declaredSymbols (table, mname, d) = Set.fromList (getTopDeclSymbols table mname d)

class ExportedSymbols a where
    exportedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ExportedSymbols (ModuleInfo l) where
    exportedSymbols i = exportedSymbols (_moduleGlobals i, _module i)

instance (Ord l, Data l) => ExportedSymbols (Global.Table, Module l) where
    exportedSymbols (table, m) =
        case getExportSpecList m of
          Nothing -> declaredSymbols (table, m)
          Just (ExportSpecList _ es) -> Set.unions (fmap (\e -> exportedSymbols (table, m, e)) es)

instance ExportedSymbols (Global.Table, Module l, ExportSpec l) where
    exportedSymbols (table, m, EVar _l qname) = Set.fromList (lookupName qname table)
    exportedSymbols (table, m, EThingWith _l _w qname _cname) = Set.fromList (lookupName qname table)
    exportedSymbols (table, m, EAbs _l _ns qname) = Set.fromList (lookupName qname table)
    exportedSymbols (table, m, EModuleContents _ name) =
        -- Find all symbols imported from name
        importedFrom name (importedSymbols (table, m))
        where importedFrom :: ModuleName l -> Set Symbol -> Set Symbol
              importedFrom name imports =
                  Set.filter (\sym -> symbolModule sym == dropAnn name) imports

class ImportedSymbols a where
    importedSymbols :: a -> Set Symbol

instance ImportedSymbols (Global.Table, Module l) where
    importedSymbols (table, m) = Set.unions (fmap (\i -> importedSymbols (table, i)) (getImports m))

instance ImportedSymbols (Global.Table, ImportDecl l) where
    importedSymbols (table, ImportDecl {importModule = mname, importSpecs = Just (ImportSpecList _ False isl)}) =
        Set.unions (fmap (\i -> importedSymbols (table, i)) isl)
    importedSymbols (table, ImportDecl {importModule = mname, importSpecs = Just (ImportSpecList _ True isl)}) =
        error "hiding"
    importedSymbols (table, ImportDecl {importModule = mname, importSpecs = Nothing}) =
        error "import module symbols"

instance ImportedSymbols (Global.Table, ImportSpec l) where
    importedSymbols (table, IVar _l name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (table, IAbs _l _space name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (table, IThingAll _l name) = Set.fromList (lookupName (nameToQName name) table)
    importedSymbols (table, IThingWith _l name _cnames) = Set.fromList (lookupName (nameToQName name) table)

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
