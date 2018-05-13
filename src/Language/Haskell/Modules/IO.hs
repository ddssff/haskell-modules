-- | Parsing is an IO operation because of the use of CPP.

module Language.Haskell.Modules.IO
    ( loadModules
    ) where

import Language.Haskell.Exts (SrcSpanInfo)
import Language.Haskell.Modules.CPP (GHCOpts)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Parse (parseAndAnnotateModules)
import Language.Haskell.Names (Scoped(..))
import Language.Haskell.TH.Instances ()
import Prelude hiding (lookup)
import System.IO (readFile)

-- | Given a list of paths to haskell modules, load the files, parse them,
-- and annotate their symbols.
loadModules :: GHCOpts -> [FilePath] -> IO [ModuleInfo (Scoped SrcSpanInfo)]
loadModules opts paths = do
  texts <- mapM readFile paths
  parseAndAnnotateModules opts (zip paths texts)
