-- | Load the symbols of a module for which we do not have source.
-- This module could be added to the @haskell-names@ package.

{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Environment
    ( buildEnvironmentForSource
    , buildEnvironmentForModules
    , buildEnvironmentForNames
    ) where

import Data.Map as Map (fromList)
import Data.Set as Set (difference, fromList, insert, Set, toList, unions)
import qualified Language.Haskell.Exts.Syntax as Exts (ModuleName(ModuleName), )
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)
import Language.Haskell.Modules.CPP (GHCOpts)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.Parse (parseModule)
import Language.Haskell.Modules.Reify (findModuleSymbols)
import Language.Haskell.Modules.Query (importedModules)
import Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)
import Language.Haskell.TH (ExpQ, listE, tupE)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax as TH (Info(..), Name(..), runIO, )

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
