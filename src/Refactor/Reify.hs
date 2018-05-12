{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Load the symbols of a module for which we do not
-- have source.

module Refactor.Reify
    ( moduleSymbols
    ) where

import qualified Language.Haskell.Exts.Syntax as Exts
import Language.Haskell.Interpreter (runInterpreter, getModuleExports{-, InterpreterError-})
import qualified Language.Haskell.Interpreter as Hint (ModuleElem(..))
import Language.Haskell.Names as Names (Symbol(..))
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (deriveLiftMany, lift)
import Language.Haskell.TH.Syntax (Dec(..), Q, lookupValueName, lookupTypeName, reify, runIO, Info(..), ModName(..), PkgName(..), Name(..), OccName(..), NameFlavour(..), NameSpace(..))
import qualified Language.Haskell.TH.Syntax as TH (Name(..), NameFlavour(..))

$(deriveLiftMany [''Hint.ModuleElem, ''Exts.ModuleName, ''Names.Symbol, ''Exts.Name])

moduleSymbols :: String -> ExpQ -- Q [Symbol]
moduleSymbols defmod = do
  -- Use the hint package to get the module's exported symbols
  modelems <- either (error . show) id <$> runIO (runInterpreter (getModuleExports defmod))
  -- Convert the hint ModuleElems to haskell-names Symbol type via
  -- template haskell.
  (symbols :: [[Symbol]]) <- mapM (moduleElemSymbols defmod) modelems
  lift (concat symbols)

moduleElemSymbols :: String -> Hint.ModuleElem -> Q [Symbol]
moduleElemSymbols defmod e = concatMap (infoToSymbols (undefined :: String -> Exts.Name ()) defmod) <$> moduleElemInfo defmod e

moduleElemInfo :: String -> Hint.ModuleElem -> Q [Info]
moduleElemInfo _defmod x@(Hint.Fun i) = do
  mname <- lookupNameWith lookupValueName i
  case mname of
    Left _ -> error $ "moduleElemInfo " ++ show x
    Right name@(Name (OccName "error")     (NameG VarName (PkgName "base") (ModName "GHC.Err"))) -> pure [VarI name undefined undefined]
    Right name@(Name (OccName "undefined") (NameG VarName (PkgName "base") (ModName "GHC.Err"))) -> pure [VarI name undefined undefined]
    Right name -> (: []) <$> reify name
#if 0
        -- This would work if you could (runQ . reify) in the IO monad
        (runIO $ catchIf (\e -> ioe_type e == UserError && ioe_description e == "Template Haskell failure")
                         (runQ $ reify name)
                         (\e -> case name of
                                  Name (OccName s) (NameG VarName (PkgName p) (ModName m)) -> pure (trace ("Caught a template haskell failure on " ++ show (V name) ++ ": " ++ show (V e)) (VarI name undefined undefined))
                                  _ -> throwM (e {ioe_description = "Template Haskell failure reifying " ++ show (V (name))})))
#endif
moduleElemInfo _defmod (Hint.Class c ms) = do
  cname <- lookupNameWith lookupTypeName c
  mnames <- mapM (\m -> lookupNameWith lookupValueName m) ms
  mapM (either (\s -> error $ "moduleElemInfo - " ++ s) reify) (cname : mnames)
moduleElemInfo _defmod (Hint.Data t fs) = do
  tname <- lookupNameWith lookupTypeName t
  fnames <- mapM (lookupNameWith lookupValueName) fs
  mapM (either (\s -> error $ "moduleElemInfo - " ++ s) reify) (tname : fnames)

#if 0
lookupNamesWith :: (String -> Q (Maybe TH.Name)) -> [String] -> Q [Either String TH.Name]
lookupNamesWith look xs =
  mapM (lookupNameWith look) xs
#endif

lookupNameWith  :: (String -> Q (Maybe TH.Name)) -> String -> Q (Either String TH.Name)
lookupNameWith look i = do
  mname <- look i
  case mname of
    Nothing -> case stripSymbol i of
                 Nothing -> return (Left i)
                 Just i' -> lookupNameWith look i'
    Just name -> return (Right name)

stripSymbol :: String -> Maybe String
stripSymbol s | length s >= 3 && head s == '(' && last s == ')' = Just (take (length s - 2) (drop 1 s))
stripSymbol _ = Nothing

infoToSymbols :: (String -> Exts.Name ()) -> String -> Info -> [Names.Symbol]
infoToSymbols symbolOrIdent defmod (VarI name _type _mdecs) =
    [Names.Value {symbolModule = thNameToModName defmod name, symbolName = thNameToExtsName symbolOrIdent name}]
infoToSymbols symbolOrIdent defmod (ClassI (ClassD _ cname _ _ _) _) =
    [Names.Class {symbolModule = thNameToModName defmod cname, symbolName = thNameToExtsName symbolOrIdent cname}]
infoToSymbols symbolOrIdent defmod (ClassOpI mname _typ cname) =
    [Names.Method {symbolModule = thNameToModName defmod mname, symbolName = thNameToExtsName symbolOrIdent mname,
                   className = thNameToExtsName symbolOrIdent cname}]
infoToSymbols symbolOrIdent defmod (TyConI (DataD _ tname _ _ _ _)) =
    [Names.Data {symbolModule = thNameToModName defmod tname, symbolName = thNameToExtsName symbolOrIdent tname}]
infoToSymbols symbolOrIdent defmod (TyConI (NewtypeD _ tname _ _ _ _)) =
    [Names.NewType {symbolModule = thNameToModName defmod tname, symbolName = thNameToExtsName symbolOrIdent tname}]
infoToSymbols symbolOrIdent defmod (DataConI cname _type tname) =
    [Names.Constructor {symbolModule = thNameToModName defmod cname, symbolName = thNameToExtsName symbolOrIdent cname,
                        typeName = thNameToExtsName symbolOrIdent tname}]
infoToSymbols symbolOrIdent defmod (TyConI (TySynD tname _ _typ)) =
    [Names.Type {symbolModule = thNameToModName defmod tname, symbolName = thNameToExtsName symbolOrIdent tname}]
infoToSymbols _ _ i = error $ "infoToSymbols - " ++ show i

thNameToExtsName :: (String -> Exts.Name ()) -> TH.Name -> Exts.Name ()
thNameToExtsName _ (TH.Name (OccName o) TH.NameS) = error ("NameS o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameQ (ModName modname))) = error ("NameQ o=" ++ show o ++ ", modname=" ++ show modname)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameU _n)) = error ("NameU o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameL _n)) = error ("NameL o=" ++ show o)
thNameToExtsName symbolOrIdent (TH.Name (OccName o) (TH.NameG VarName (PkgName _) (ModName _))) = symbolOrIdent o
thNameToExtsName symbolOrIdent (TH.Name (OccName o) (TH.NameG DataName (PkgName _) (ModName _))) = symbolOrIdent o
thNameToExtsName symbolOrIdent (TH.Name (OccName o) (TH.NameG TcClsName (PkgName _) (ModName _))) = symbolOrIdent o

thNameToModName :: String -> TH.Name -> Exts.ModuleName ()
thNameToModName d (TH.Name _ TH.NameS) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameQ (ModName modname))) = Exts.ModuleName () modname
thNameToModName d (TH.Name _ (TH.NameU _)) = Exts.ModuleName () d
thNameToModName d (TH.Name _ (TH.NameL _)) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameG VarName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG DataName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG TcClsName (PkgName _) (ModName modname))) = Exts.ModuleName () modname

{-
symbolOrIdent :: String -> Exts.Name ()
symbolOrIdent = Ident ()
-}
