{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TupleSections, TypeFamilies #-}

-- | Ask questions about modules - unused imports etc.

module Language.Haskell.Modules.Query
    ( DeclaredSymbols(declaredSymbols)
    , ExportedSymbols(exportedSymbols')
    , ImportedModules(importedModules)
    , ImportedSymbols(importedSymbols)
    , ReferencedNames(referencedNames)
    , referencedSymbols
    , lookupNames
    ) where

--import Debug.Trace (trace)
import Data.Data (Data)
--import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)
import Data.Set as Set (difference, empty, fromList, Set, singleton, union, unions)
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

instance (Ord l, Data l) => DeclaredSymbols (Environment, Module l) where
    declaredSymbols (env, m) = Set.unions (fmap (\d -> declaredSymbols (env, m, d)) (getModuleDecls m))

instance (Ord l, Data l) => DeclaredSymbols (Environment, Module l, Decl l) where
    declaredSymbols (env, m, d) = Set.fromList (getTopDeclSymbols (moduleGlobals env m) (getModuleName m) d)

-- | This duplicates the function of 'exportedSymbols'
class ExportedSymbols a where
    exportedSymbols' :: a -> Set Symbol

instance ExportedSymbols (Environment, ModuleName (Scoped l)) where
    -- Find all the of the module's symbols in the global symbol table
    exportedSymbols' (env, name) =
        -- It might be worth returning a Maybe to preserve the fact
        -- that the module is not present in the environment.
        maybe Set.empty Set.fromList (Map.lookup (dropAnn name) env)

instance (l ~ SrcSpanInfo) => ExportedSymbols (Environment, Module (Scoped l)) where
    exportedSymbols' (env, m) = Set.fromList (exportedSymbols (moduleTable (importTable env m) (dropAnn m)) m)

instance (l ~ SrcSpanInfo) => ExportedSymbols (Environment, Module (Scoped l), ExportSpec (Scoped l)) where
    exportedSymbols' (_env, _m, espec) = Set.unions (fmap (symbols . ann) (gFind espec :: [Name (Scoped l)]))

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

instance Data l => ImportedSymbols (Environment, Module (Scoped l)) where
    importedSymbols (env, m) = Set.unions (fmap (\i -> importedSymbols (env, m, i)) (getImports m))

instance Data l => ImportedSymbols (Environment, Module (Scoped l), ImportDecl (Scoped l)) where
    importedSymbols (env, m, d@ImportDecl {importModule = _mname, importAs = _aname, importSpecs = Just (ImportSpecList _ False _isl)}) =
        Set.unions (fmap (\s -> importedSymbols (env, m, d, s)) (gFind d :: [ImportSpec (Scoped l)]))
        -- Set.unions (fmap (\ispec -> importedSymbols (env, m, mname, aname, ispec)) isl)
    importedSymbols (env, m, d@ImportDecl {importModule = mname, importAs = _aname, importSpecs = Just (ImportSpecList _ True isl)}) =
        -- All the symbols the module exports other than the ones in the list
        Set.difference (exportedSymbols' (env, mname)) (Set.unions (fmap (\s -> importedSymbols (env, m, d, s)) isl))
    importedSymbols (env, _m, ImportDecl {importModule = mname, importSpecs = Nothing}) =
        -- All the symbols in a module
        exportedSymbols' (env, mname)

instance Data l => ImportedSymbols (Environment, Module (Scoped l), ImportDecl (Scoped l), ImportSpec (Scoped l)) where
    importedSymbols (_env, _m, _idecl, ispec) = Set.unions (fmap (symbols . ann) (gFind ispec :: [Name (Scoped l)]))

symbols :: Scoped l -> Set Symbol
symbols x = Set.fromList (gFind (fmap (const ()) x) :: [Symbol])

-- | Return the set of 'QName' used in an object such as a declaration.
-- One then builds a symbol table for the and looks these up
-- For a module to be valid, each of these must either be imported or
-- declared.
class ReferencedNames a where
  referencedNames :: a -> Set (QName (Scoped SrcSpanInfo))

instance ReferencedNames a => ReferencedNames [a] where
    referencedNames l = foldr (\a r -> Set.union r (referencedNames a)) Set.empty l
instance ReferencedNames a => ReferencedNames (Maybe a) where
    referencedNames = maybe Set.empty referencedNames

instance (l ~ SrcSpanInfo) => ReferencedNames (Module (Scoped l)) where
    referencedNames m =
        Set.unions (maybe Set.empty (\e -> referencedNames e) (getExportSpecList m) :
                    fmap (\d -> referencedNames d) (getModuleDecls m))

instance (l ~ SrcSpanInfo) => ReferencedNames (ExportSpecList (Scoped l)) where
    referencedNames x = Set.fromList names
        where names = fmap nameToQName (gFind x :: [Name (Scoped l)]) ++ gFind x :: [QName (Scoped l)]

instance (l ~ SrcSpanInfo) => ReferencedNames (Decl (Scoped l)) where
    -- The symbol being declared by a type signature does not count as
    -- a reference, if a symbol is declared in a module and has an
    -- accompanying signature it still may be "Defined but not used".
    referencedNames (TypeSig _ _names typ) =
        Set.fromList (fmap nameToQName (gFind typ :: [Name (Scoped l)]) ++ gFind typ :: [QName (Scoped l)])
    referencedNames x =
        Set.fromList (fmap nameToQName (gFind x :: [Name (Scoped l)]) ++ gFind x :: [QName (Scoped l)])

lookupNames :: Set (QName (Scoped l)) -> Table -> Set Symbol
lookupNames names table = foldl (\r name -> Set.union r (Set.fromList (lookupName name table))) Set.empty names

referencedSymbols :: (l ~ SrcSpanInfo) => Environment -> Module (Scoped l) -> Set Symbol
referencedSymbols env m = lookupNames (referencedNames m) (moduleGlobals env m)
