-- | Parsing is an IO operation because of the use of CPP.

{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Modules.IO
    ( loadModules
    ) where

import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Language.Haskell.Exts (SrcSpanInfo)
import Language.Haskell.Modules.CPP (GHCOpts)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Parse (parseAndAnnotateModules)
import Language.Haskell.Names (Environment, Scoped(..))
import Language.Haskell.TH.Instances ()
import Prelude hiding (lookup)
import System.IO (readFile)

-- | Given a list of paths to haskell modules, load the files, parse them,
-- and annotate their symbols.
loadModules :: (MonadState Environment m, MonadIO m) => GHCOpts -> [FilePath] -> m [ModuleInfo (Scoped SrcSpanInfo)]
loadModules opts paths = do
  texts <- liftIO $ mapM readFile paths
  parseAndAnnotateModules opts (zip paths texts)
