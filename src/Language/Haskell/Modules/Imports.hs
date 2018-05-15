{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Imports
    ( buildEnvironmentForSource
    , buildEnvironmentForModules
    , buildEnvironmentForNames
    ) where

import Data.Set as Set (difference, insert, Set, toList, unions)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax ({-ImportDecl(..),-} ModuleName(..))
import Language.Haskell.Modules.CPP (GHCOpts)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.Parse (parseModule)
import Language.Haskell.Modules.Query (importedModules)
import Language.Haskell.Modules.Reify (findModuleSymbols)
import Language.Haskell.Modules.Utils (mapFromList, setFromList)
import Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)
import Language.Haskell.TH (ExpQ, Info, listE, Name, runIO, tupE)
import Language.Haskell.TH.Lift (lift)
import System.IO (readFile)

buildEnvironmentForSource :: Int -> GHCOpts -> (Name -> Maybe Info) -> [FilePath] -> ExpQ -- IO Environment
buildEnvironmentForSource verbosity opts danger paths = do
  texts <- runIO (mapM readFile paths)
  mods <- runIO (mapM (uncurry (parseModule opts)) (zip paths texts))
  buildEnvironmentForModules verbosity danger mods

buildEnvironmentForModules :: forall l. (l ~ SrcSpanInfo) => Int -> (Name -> Maybe Info) -> [ModuleInfo l] -> ExpQ -- IO Environment
buildEnvironmentForModules verbosity danger mods = do
  buildEnvironmentForNames verbosity danger
    (Set.difference
       (Set.insert
          -- There shouldn't be any reason not to import Prelude into the Environment.
          (ModuleName () "Prelude")
          (Set.unions (fmap (importedModules . _module) mods)))
       (setFromList (fmap (dropAnn . getModuleName . _module) mods)))

-- | The Environment is the thing that imports reach out to.  This
-- builds an environment that contains all the modules imported by any
-- of the modules in the list.
buildEnvironmentForNames ::
       Int -- ^ Verbosity - if you encounter a dangerous name that your dangerous name handler
           -- doesn't know, set this to 1 to get a trace of the symbols being reified.
    -> (Name -> Maybe Info) -- ^ Dangerous name handler
    -> Set (ModuleName ()) -- ^ Use 'importedModules' to convert things to this set of imported modules
    -> ExpQ -- Q Environment
buildEnvironmentForNames verbosity danger mnames =
    [|mapFromList $(listE (fmap (\name -> tupE [lift name,
                                                findModuleSymbols verbosity danger (getModuleNameString name)])
                             (Set.toList mnames)))|]
{-
    where
      mods :: Set (ModuleName ())
      mods = Set.difference (setFromList (fmap getImportModName (fmap dropAnn ims) ++ fmap dropAnn extra)) pms
      ims :: [ImportDecl l]
      ims = concatMap getImports ms
      pms :: Set (ModuleName ())
      pms = setFromList (fmap (dropAnn . getModuleName) ms)

getImportModName :: ImportDecl l -> ModuleName l
getImportModName (ImportDecl _ m _ _ _ _ _ _) = m
-}

getModuleNameString :: ModuleName l -> String
getModuleNameString (ModuleName _ s) = s

-- | Build a predicate that removes unused imports.
-- cleanImports :: Environment -> ModuleInfo (Scoped l) -> 
