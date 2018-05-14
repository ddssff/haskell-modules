{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-deprecations #-}

module Environment where

import Control.Lens (over)
import Data.Default (def)
import Data.Graph.Inductive (grev)
import Data.List (sortBy)
import Data.Map as Map (fromList, lookup, Map)
import Data.Set as Set (difference, empty, insert, Set, singleton, toList, unions)
import Language.Haskell.Exts (Name(Ident), SrcSpanInfo)
import Language.Haskell.Exts.Syntax (ImportDecl(..), Module, ModuleName(..))
import Language.Haskell.Interpreter (runInterpreter, getModuleExports{-, InterpreterError-})
import Language.Haskell.Modules.CPP ({-ghcOptions,-} hsSourceDirs)
import Language.Haskell.Modules.Danger (dangerous)
import Language.Haskell.Modules.FGL (components)
import Language.Haskell.Modules.Imports (buildEnvironment)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.IO (loadModules)
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Query (importedModules)
import Language.Haskell.Modules.Reify (findModuleSymbols)
import Language.Haskell.Modules.Utils (setFromList)
import Language.Haskell.Names (Environment, loadBase, ppSymbol, Scoped(..), Symbol(..))
import Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)
import Language.Haskell.TH.Syntax (lift, runIO)
import Language.Haskell.Exts.Syntax (Name(..))
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (ModName(..), PkgName(..))
import Prelude hiding (lookup)
import System.FilePath ((</>))
import System.IO (readFile, writeFile)
import Test.HUnit hiding (path)
import Control.Monad (msum)

-- These imports are required to reify the symbols imported by the
-- modules we parse and annotate.
import qualified Control.Lens
import qualified Control.Monad -- (msum, when)
import qualified Control.Monad.Catch
import qualified Control.Monad.RWS
import qualified Control.Monad.State
import qualified Data.Char
import qualified Data.Data
import qualified Data.Default
import qualified Data.Foldable
import qualified Data.Generics
import qualified Data.Graph.Inductive
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Set
import qualified Data.Tuple
import qualified Data.Version
import qualified Debug.Show
import qualified Distribution.Compat.ReadP
import qualified Distribution.Package
import qualified Distribution.Text
import qualified GHC.IO.Exception
import qualified Language.Haskell.Exts
import qualified Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.CPP
import qualified Language.Haskell.Exts.Extension
import qualified Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Interpreter
import qualified Language.Haskell.Names
import qualified Language.Haskell.Names.GlobalSymbolTable
import qualified Language.Haskell.Names.Imports
import qualified Language.Haskell.Names.ModuleSymbols
import qualified Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.TH
import qualified Language.Haskell.TH.Lift
import qualified Language.Haskell.TH.Syntax
import qualified Language.Preprocessor.Cpphs
import qualified Options.Applicative
import qualified Prelude
import qualified System.IO
import qualified Test.HUnit
import qualified Text.PrettyPrint.HughesPJClass

tests :: Test
tests = TestList [test1, test2]

test1 :: Test
test1 = TestCase (assertEqual "buildEnvironment 1"
                    (Map.fromList [(ModuleName () "Prelude", $(findModuleSymbols 0 dangerous "Prelude"))])
                    env1)

env1 :: Environment
env1 = $(buildEnvironment 0 dangerous (Set.singleton (ModuleName () "Prelude")))

test2 :: Test
test2 = TestCase (assertEqual "buildEnvironment 2"
                    -- Remove fmap length to see the whole ugly story
                    (Map.fromList [(ModuleName () "Control.Lens",838),
                                   (ModuleName () "Control.Monad",42),
                                   (ModuleName () "Control.Monad.Catch",34),
                                   (ModuleName () "Control.Monad.RWS",115),
                                   (ModuleName () "Control.Monad.State",69),
                                   (ModuleName () "Data.Char",63),
                                   (ModuleName () "Data.Data",106),
                                   (ModuleName () "Data.Default",2),
                                   (ModuleName () "Data.Foldable",37),
                                   (ModuleName () "Data.Generics",188),
                                   (ModuleName () "Data.Graph.Inductive",293),
                                   (ModuleName () "Data.List",116),
                                   (ModuleName () "Data.Map",113),
                                   (ModuleName () "Data.Maybe",12),
                                   (ModuleName () "Data.Monoid",32),
                                   (ModuleName () "Data.Set",54),
                                   (ModuleName () "Data.Tuple",5),
                                   (ModuleName () "Data.Version",7),
                                   (ModuleName () "Debug.Show",2),
                                   (ModuleName () "Distribution.Compat.ReadP",34),
                                   (ModuleName () "Distribution.Package",32),
                                   (ModuleName () "Distribution.Text",6),
                                   (ModuleName () "GHC.IO.Exception",65),
                                   (ModuleName () "Instances.TH.Lift",0),
                                   (ModuleName () "Language.Haskell.Exts",978),
                                   (ModuleName () "Language.Haskell.Exts.CPP",24),
                                   (ModuleName () "Language.Haskell.Exts.Comments",5),
                                   (ModuleName () "Language.Haskell.Exts.Extension",123),
                                   (ModuleName () "Language.Haskell.Exts.Parser",52),
                                   (ModuleName () "Language.Haskell.Exts.SrcLoc",44),
                                   (ModuleName () "Language.Haskell.Exts.Syntax",491),
                                   (ModuleName () "Language.Haskell.Interpreter",298),
                                   (ModuleName () "Language.Haskell.Names",52),
                                   (ModuleName () "Language.Haskell.Names.GlobalSymbolTable",13),
                                   (ModuleName () "Language.Haskell.Names.Imports",2),
                                   (ModuleName () "Language.Haskell.Names.ModuleSymbols",3),
                                   (ModuleName () "Language.Haskell.Names.SyntaxUtils",21),
                                   (ModuleName () "Language.Haskell.TH",596),
                                   (ModuleName () "Language.Haskell.TH.Instances",0),
                                   (ModuleName () "Language.Haskell.TH.Lift",8),
                                   (ModuleName () "Language.Haskell.TH.Syntax",483),
                                   (ModuleName () "Language.Preprocessor.Cpphs",51),
                                   (ModuleName () "Options.Applicative",156),
                                   (ModuleName () "Prelude",253),
                                   (ModuleName () "System.IO",106),
                                   (ModuleName () "Test.HUnit",52),
                                   (ModuleName () "Text.PrettyPrint.HughesPJClass",77)])
                    (fmap length env2))

-- | Build an 'Environment' containing all the symbols imported
-- by a list of modules, in this case the modules of this library.
env2 :: Environment
env2 =
    $(do mods <- runIO (loadModules
                          (over hsSourceDirs (++ ["/home/dsf/git/haskell-modules/src", "/home/dsf/git/haskell-modules/tests"]) def)
                            (fmap ("src/Language/Haskell/Modules" </>)
                               ["Utils.hs", "SrcLoc.hs", "Reify.hs", "Orphans.hs", "Info.hs", "FGL.hs", "Split.hs",
                                "CPP.hs", "Parse.hs", "Query.hs", "Render.hs", "IO.hs", "Danger.hs", "Imports.hs"]))
         -- These symbols are bound to declarations that use
         -- implicit parameters, and are therefore impossible to
         -- reify (as of ghc-8.0 thru 8.4.)
         buildEnvironment 0 dangerous (Set.difference
                                         (Set.insert (ModuleName () "Prelude") (Set.unions (fmap (importedModules . _module) mods)))
                                         (setFromList (fmap (dropAnn . getModuleName . _module) mods))))
