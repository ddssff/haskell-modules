{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}

-- | Ask questions about modules - unused imports etc.

module Language.Haskell.Modules.Query
    ( DeclaredSymbols(declaredSymbols)
    , ExportedSymbols(exportedSymbols)
    , ImportedModules(importedModules)
    , ImportedSymbols(importedSymbols)
    , referencedSymbols
    ) where

import Debug.Trace (trace)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)
import Data.Set as Set (difference, empty, filter, fromList, Set, singleton, unions)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.Info
import Language.Haskell.Modules.Utils (gFind)
import Language.Haskell.Names (Scoped(..), Symbol(..), Environment)
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName, Table)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils

class DeclaredSymbols a where
    declaredSymbols :: a -> Set Symbol

instance (Ord l, Data l) => DeclaredSymbols (Environment, ModuleInfo l) where
    declaredSymbols (env, i) = declaredSymbols (env, moduleGlobals env (_module i), _module i)

instance (Ord l, Data l) => DeclaredSymbols (Environment, Global.Table, Module l) where
    declaredSymbols (env, table, m) = Set.unions (fmap (\d -> declaredSymbols (env, table, getModuleName m, d)) (getModuleDecls m))

instance (Ord l, Data l) => DeclaredSymbols (Environment, Global.Table, ModuleName l, Decl l) where
    declaredSymbols (_env, table, mname, d) = Set.fromList (getTopDeclSymbols table mname d)

class ExportedSymbols a where
    exportedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ExportedSymbols (Environment, ModuleInfo l) where
    exportedSymbols (env, i) = exportedSymbols (env, moduleGlobals env (_module i), _module i)

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

-- | Symbols that are either imported or declared
class ImportedSymbols a where
    importedSymbols :: a -> Set Symbol

instance (Ord l, Data l) => ImportedSymbols (Environment, ModuleInfo l) where
    importedSymbols (env, i) = importedSymbols (env, moduleGlobals env (_module i), _module i)

instance (Ord l, Data l) => ImportedSymbols (Environment, Global.Table, Module l) where
    importedSymbols (env, table, m) = Set.unions (fmap (\i -> importedSymbols (env, table, i)) (getImports m))

instance (Ord l, Data l) => ImportedSymbols (Environment, Global.Table, ImportDecl l) where
    importedSymbols (env, table, ImportDecl {importModule = mname, importAs = aname, importSpecs = Just (ImportSpecList _ False isl)}) =
        Set.unions (fmap (\ispec -> importedSymbols (env, table, mname, aname, ispec)) isl)
    importedSymbols (env, table, ImportDecl {importModule = mname, importAs = aname, importSpecs = Just (ImportSpecList _ True isl)}) =
        -- All the symbols the module exports other than the ones in the list
        Set.difference (exportedSymbols (env, mname)) (Set.unions (fmap (\i -> importedSymbols (env, table, mname, aname, i)) isl))
    importedSymbols (env, _table, ImportDecl {importModule = mname, importSpecs = Nothing}) =
        -- All the symbols in a module
        exportedSymbols (env, mname)

instance ImportedSymbols (Environment, Global.Table, ModuleName l, Maybe (ModuleName l), ImportSpec l) where
    importedSymbols (_env, table, mname, aname, IVar _l name) = Set.fromList (lookupName (toQName (fromMaybe mname aname) name) table)
    importedSymbols (_env, table, mname, aname, IAbs _l _space name) = Set.fromList (lookupName (toQName (fromMaybe mname aname) name) table)
    importedSymbols (_env, table, mname, aname, IThingAll _l name) = Set.fromList (lookupName (toQName (fromMaybe mname aname) name) table)
    importedSymbols (_env, table, mname, aname, IThingWith _l name _cnames) = Set.fromList (lookupName (toQName (fromMaybe mname aname) name) table)

toQName :: ModuleName l -> Name l -> QName ()
toQName mname name = Qual () (dropAnn mname) (dropAnn name)

-- | Every symbol used in a declaration or an export.  For a module to
-- be valid, each of these must either be imported or declared.
class ReferencedSymbols a where
  referencedSymbols :: a -> Set Symbol

instance ReferencedSymbols (Environment, ModuleInfo (Scoped SrcSpanInfo)) where
    referencedSymbols (env, i) = referencedSymbols (env, moduleGlobals env (_module i), _module i)

instance ReferencedSymbols (Environment, Global.Table, Module (Scoped SrcSpanInfo)) where
    referencedSymbols (env, table, m) =
        Set.unions (maybe Set.empty (\e -> referencedSymbols (env, table, e)) (getExportSpecList m) :
                    fmap (\d -> referencedSymbols (env, table, getModuleName m, d)) (getModuleDecls m))

instance ReferencedSymbols (Environment, Global.Table, ExportSpecList (Scoped SrcSpanInfo)) where
    referencedSymbols (env, table, x) =
        Set.fromList (concatMap (`lookupName` table) names)
        where names = fmap nameToQName (gFind x :: [Name (Scoped SrcSpanInfo)]) ++ gFind x :: [QName (Scoped SrcSpanInfo)]

instance ReferencedSymbols (Environment, Global.Table, ModuleName (Scoped SrcSpanInfo), Decl (Scoped SrcSpanInfo)) where
    -- A type signature does not count as a reference, if a symbol
    -- is declared in a module and has an accompanying signature
    -- it still may be "Defined but not used".
    referencedSymbols (env, table, mname, x@(TypeSig {})) = Set.empty
    referencedSymbols (env, table, mname, x) =
        (Set.difference
          ({-t1 x-} (Set.fromList (concatMap (`lookupName` table) names)))
          ({-t2 x-} (declaredSymbols (env, table, mname, x))))
        where names = fmap nameToQName (gFind x :: [Name (Scoped SrcSpanInfo)]) ++ gFind x :: [QName (Scoped SrcSpanInfo)]

-- t1 d r = trace ("referenced by " ++ show d ++ ": " ++ show r) r
-- t2 d r = trace ("declared by " ++ show d ++ ": " ++ show r) r

{-
referencedSymbols d =
  Set.fromList (concatMap (`lookupName` table) names)
  where names = fmap nameToQName (gFind d :: [Name (Scoped SrcSpanInfo)]) ++ gFind d :: [QName (Scoped SrcSpanInfo)]
-}


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
