-- | Parsing is an IO operation because of the use of CPP.

module Refactor.IO
    ( loadModules
    ) where

import Control.Monad (when)
import Data.Default (def)
import Data.Function (on)
import Data.Graph.Inductive (grev)
import Data.List (sortBy)
import Data.Map as Map (lookup, Map)
import Data.Set as Set (difference, fromList, toList, unions)
import Language.Haskell.Exts (Name(Ident), SrcSpanInfo)
import Language.Haskell.Exts.Syntax (ImportDecl(..), Module, ModuleName(..))
import qualified Language.Haskell.Exts.Syntax as Exts
import Language.Haskell.Interpreter (runInterpreter, getModuleExports{-, InterpreterError-})
import qualified Language.Haskell.Interpreter as Hint (ModuleElem(..))
import Language.Haskell.Names (loadBase, ppSymbol, Scoped(..), Symbol(..))
import Language.Haskell.TH.Syntax (lift, runIO)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.Exts.Syntax (Name(..))
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (ModName(..), PkgName(..))
import Prelude hiding (lookup)
import Refactor.FGL (components)
import Refactor.Imports (buildEnvironment)
import Refactor.Orphans ()
import Refactor.Parse (parseAndAnnotateModules)
import Refactor.Reify (findModuleSymbols)
import Refactor.Render (renderModule)
import Refactor.Split (bisect, declares, DeclGroup(unDecs), withDecomposedModule)
import System.IO (readFile, writeFile)
import Test.HUnit hiding (path)
import Refactor.CPP (GHCOpts)
import Refactor.Info (ModuleInfo(..))

-- | Given a list of paths to haskell modules, load the files, parse them,
-- and annotate their symbols.
loadModules :: GHCOpts -> [FilePath] -> IO [ModuleInfo (Scoped SrcSpanInfo)]
loadModules opts paths = do
  texts <- mapM readFile paths
  parseAndAnnotateModules opts (zip paths texts)
