-- | Load the symbols of a module for which we do not have source.
-- This module could be added to the @haskell-names@ package.

{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Reify
    ( buildEnvironmentForSource
    , buildEnvironmentForModules
    , buildEnvironmentForNames
    , findModuleSymbols
    ) where

import Control.Monad (msum)
import Control.Monad.State (modify, runStateT, StateT)
import Data.Map as Map (fromList)
import Data.Set as Set (difference, fromList, insert, member, Set, toList, unions)
import qualified Language.Haskell.Exts.Syntax as Exts (ModuleName(ModuleName), Name(Symbol, Ident))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)
import Language.Haskell.Interpreter as Hint (runInterpreter, getModuleExports, ModuleElem(..))
import Language.Haskell.Modules.CPP (GHCOpts)
import Language.Haskell.Modules.Danger (reify')
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Parse (parseModule)
import Language.Haskell.Modules.Query (importedModules)
import Language.Haskell.Modules.Utils (singleton)
import Language.Haskell.Names as Names (Symbol(..))
import Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)
import Language.Haskell.TH (ExpQ, listE, runQ, tupE)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax as TH
    (Dec(..), Info(..), lookupValueName, lookupTypeName, ModName(..),
     Name(..), NameFlavour(..), NameSpace(..), OccName(..), PkgName(..), Q, runIO, TypeFamilyHead(..))

-- | Build the 'Environment' required by some source files.
buildEnvironmentForSource :: Int -> GHCOpts -> (Name -> Maybe Info) -> [FilePath] -> ExpQ -- IO Environment
buildEnvironmentForSource verbosity opts danger paths = do
  texts <- runIO (mapM readFile paths)
  mods <- runIO (mapM (uncurry (parseModule opts)) (zip paths texts))
  buildEnvironmentForModules verbosity danger (fmap _module mods)

-- | Build the 'Environment' required by the parsed modules.
buildEnvironmentForModules :: forall l. (l ~ SrcSpanInfo) => Int -> (Name -> Maybe Info) -> [Module l] -> ExpQ -- IO Environment
buildEnvironmentForModules verbosity danger mods = do
  buildEnvironmentForNames verbosity danger
    (Set.difference
       (Set.insert
          -- There shouldn't be any reason not to import Prelude into the Environment.
          (Exts.ModuleName () "Prelude")
          (Set.unions (fmap importedModules mods)))
       (Set.fromList (fmap (dropAnn . getModuleName) mods)))

-- | Build the 'Environment' required by the named modules.  This builds
-- an environment that contains all the modules imported by any of the
-- modules in the list.
buildEnvironmentForNames ::
       Int -- ^ Verbosity - if you encounter a dangerous name that your dangerous name handler
           -- doesn't know, set this to 1 to get a trace of the symbols being reified.
    -> (Name -> Maybe Info) -- ^ Dangerous name handler
    -> Set (Exts.ModuleName ()) -- ^ Use 'importedModules' to convert things to this set of imported modules
    -> ExpQ -- Q Environment
buildEnvironmentForNames verbosity danger mnames =
    [|Map.fromList $(listE (fmap (\name@(Exts.ModuleName _ s) ->
                                     tupE [lift name, findModuleSymbols verbosity danger s])
                             (Set.toList mnames)))|]

-- | Given a module name, get its top level 'Symbol' list.  This is done
-- using the geModuleExports function from the @hint@ package, then
-- using @template-haskell@'s 'lookupValueName' and 'reify' functions to
-- learn everything about those symbols, and then converting that info
-- to the @haskell-names@ 'Symbol' type.  The results should compare to
-- the output of 'loadBase'.
findModuleSymbols :: Int -> (Name -> Maybe Info) -> String -> ExpQ -- Q [Symbol]
findModuleSymbols verbosity special defmod = do
  modelems <- either (error . show) id <$> runIO (runInterpreter (getModuleExports defmod))
  symbols <- concat <$> mapM (moduleElemSymbols verbosity special defmod) modelems :: Q [Symbol]
  lift symbols

moduleElemSymbols :: Int -> (Name -> Maybe Info) -> String -> Hint.ModuleElem -> Q [Symbol]
moduleElemSymbols verbosity special defmod e = do
    (infos, syms) <- runStateT (do names <- (moduleElemNames defmod) e
                                   mapM (nameInfo verbosity special defmod) names) mempty
    return $ concatMap (infoToSymbols defmod syms) infos

-- | We have a ModuleElem, which represents some symbols imported from
-- defmod.  These symbols may not actually be located in defmod,
-- defmod might have re-exported them.
moduleElemNames :: String -> Hint.ModuleElem -> StateT (Set String) Q [Either String Name]
moduleElemNames defmod (Hint.Fun i) =
    singleton <$> lookupNameWith defmod lookupValueName i
moduleElemNames defmod (Hint.Class c ms) = do
  cname <- lookupNameWith defmod lookupTypeName c
  mnames <- mapM (\m -> lookupNameWith defmod lookupValueName m) ms
  return (cname : mnames)
moduleElemNames defmod (Hint.Data t fs) = do
  tname <- lookupNameWith defmod lookupTypeName t
  fnames <- mapM (lookupNameWith defmod lookupValueName) fs
  return (tname : fnames)

nameInfo :: Int -> (Name -> Maybe Info) -> String -> Either String Name -> StateT (Set String) Q Info
nameInfo verbosity special defmod name =
  either (\s -> error $ "nameInfo - could not reify " ++ s ++ " in " ++ defmod ++ " - is it imported?")
         (runQ . reify' verbosity special)
         name

lookupNameWith  :: String -> (String -> Q (Maybe TH.Name)) -> String -> StateT (Set String) Q (Either String TH.Name)
lookupNameWith defmod look i = do
  mname1 <- runQ $ look ({-t3-} (defmod ++ "." ++ i))
  mname2 <- runQ $ look ({-t4-} i)
  case msum ({-t5-} [mname1, mname2]) of
    Nothing -> case stripSymbol i of
                 Nothing -> return (Left i)
                 Just i' -> modify (Set.insert i') >> lookupNameWith defmod look i'
    Just name -> return (Right name)

-- t3 x = trace ("look3 " ++ show x) x
-- t4 x = trace ("look4 " ++ show x) x
-- t5 x = trace ("look5 " ++ show x) x

stripSymbol :: String -> Maybe String
stripSymbol s | length s >= 3 && head s == '(' && last s == ')' = Just (take (length s - 2) (drop 1 s))
stripSymbol _ = Nothing

infoToSymbols :: String -> Set String -> Info -> [Names.Symbol]
infoToSymbols defmod syms (VarI name _type _mdecs) =
    [Names.Value
       {symbolModule = thNameToModName defmod name,
        symbolName = thNameToExtsName syms name}]
infoToSymbols defmod syms (ClassI (ClassD _ cname _ _ _) _) =
    [Names.Class
       {symbolModule = thNameToModName defmod cname,
        symbolName = thNameToExtsName syms cname}]
infoToSymbols defmod syms (ClassOpI mname _typ cname) =
    [Names.Method
       {symbolModule = thNameToModName defmod mname,
        symbolName = thNameToExtsName syms mname,
        className = thNameToExtsName syms cname}]
infoToSymbols defmod syms (TyConI (DataD _ tname _ _ _ _)) =
    [Names.Data
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (TyConI (NewtypeD _ tname _ _ _ _)) =
    [Names.NewType
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (DataConI cname _type tname) =
    [Names.Constructor
       {symbolModule = thNameToModName defmod cname,
        symbolName = thNameToExtsName syms cname,
        typeName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (TyConI (TySynD tname _ _typ)) =
    [Names.Type
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
-- I'm sure these will show up shortly :-(
infoToSymbols defmod syms (FamilyI (OpenTypeFamilyD (TypeFamilyHead tname _ _ _)) insts) =
    [Names.TypeFam
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname,
        associate = findAssociateName insts}]
infoToSymbols defmod syms (FamilyI (ClosedTypeFamilyD (TypeFamilyHead tname _ _ _) _) insts) =
    [Names.TypeFam
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname,
        associate = findAssociateName insts}]
infoToSymbols defmod syms (FamilyI (DataFamilyD tname _ _) insts) =
    [Names.DataFam
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname,
        associate = findAssociateName insts}]
infoToSymbols _ _ i@(FamilyI _ _) = error $ "unimplemented - infoToSymbols " ++ show i
infoToSymbols _ _ i@(ClassI _ _) = error $ "unimplemented - infoToSymbols " ++ show i
infoToSymbols _ _ i@(TyConI _) = error $ "unimplemented - infoToSymbols " ++ show i
infoToSymbols _ _ i@(PrimTyConI _ _ _) = error $ "unimplemented - infoToSymbols " ++ show i
infoToSymbols _ _ i@(TyVarI _ _) = error $ "unimplemented - infoToSymbols " ++ show i

findAssociateName :: [Dec] -> Maybe (Exts.Name ())
findAssociateName _ = Nothing

thNameToExtsName :: Set String -> TH.Name -> Exts.Name ()
thNameToExtsName _ (TH.Name (OccName o) TH.NameS) = error ("NameS o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameQ (ModName modname))) = error ("NameQ o=" ++ show o ++ ", modname=" ++ show modname)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameU _n)) = error ("NameU o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameL _n)) = error ("NameL o=" ++ show o)
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG VarName (PkgName _) (ModName _))) = symbolOrIdent syms o
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG DataName (PkgName _) (ModName _))) = symbolOrIdent syms o
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG TcClsName (PkgName _) (ModName _))) = symbolOrIdent syms o

thNameToModName :: String -> TH.Name -> Exts.ModuleName ()
thNameToModName d (TH.Name _ TH.NameS) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameQ (ModName modname))) = Exts.ModuleName () modname
thNameToModName d (TH.Name _ (TH.NameU _)) = Exts.ModuleName () d
thNameToModName d (TH.Name _ (TH.NameL _)) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameG VarName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG DataName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG TcClsName (PkgName _) (ModName modname))) = Exts.ModuleName () modname

symbolOrIdent :: Set String -> String -> Exts.Name ()
symbolOrIdent syms s | Set.member s syms = Exts.Symbol () s
symbolOrIdent _ s = Exts.Ident () s
