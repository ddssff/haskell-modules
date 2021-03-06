{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Query where

import Control.Monad.State (runStateT)
import Data.Default (def)
--import Data.Graph.Inductive (grev)
import Data.Set as Set (difference, filter, fromList, map, union, unions)
import Language.Haskell.Exts (Name(..), QName(..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Language.Haskell.Names (Environment, ppSymbol, Scoped, Symbol(..))
import Language.Haskell.TH.Instances ()
import Prelude hiding (lookup)
-- import Language.Haskell.Modules.Clean (cleanImports)
import Language.Haskell.Modules.FGL (components)
import Language.Haskell.Modules.Graphs (DeclGroup(unDecs))
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.IO (loadModules)
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Parse (parseAndAnnotateModules)
import Language.Haskell.Modules.Query (declaredSymbols, exportedSymbols', importedSymbols, referencedNames, referencedSymbols)
import Language.Haskell.Modules.Render (renderModule)
import Language.Haskell.Modules.Split (bisect, withDecomposedModule, cleanImports)
import Language.Haskell.Names.Exports (exportedSymbols)
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)
import System.FilePath ((</>))
import System.IO (readFile, writeFile)
import Test.HUnit hiding (path)

import Environment (env2)

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test5b, test5c, test6]

paths = fmap ("src/Language/Haskell/Modules" </>)
          ["Utils.hs",
           "SrcLoc.hs",
           "Orphans.hs",
           "Info.hs",
           "Query.hs",
           "FGL.hs",
           "Graphs.hs",
           "Split.hs",
           "Danger.hs",
           "CPP.hs",
           "Parse.hs",
           "IO.hs",
           "Render.hs",
           "Reify.hs",
           "Environment.hs"]

load :: IO ([ModuleInfo (Scoped SrcSpanInfo)], Environment)
load = runStateT (loadModules def paths) env2

loadMyModule :: String -> IO (ModuleInfo (Scoped SrcSpanInfo), Environment)
loadMyModule nick = do
  let name = "Language.Haskell.Modules." ++ nick
  (mods, env') <- load
  case Prelude.filter (\x -> dropAnn (getModuleName (_module x)) == ModuleName () name) mods of
    [i] -> return (i, env')
    [] -> error $ "Could not load module " ++ name

test1 :: Test
test1 =
  TestCase $ do (i, env') <- loadMyModule "Render"
                assertEqual "declared 1"
                  (Set.fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "initRenderInfo"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep'"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep''"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keepV"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "labelled"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "prefixed"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "renderModule"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "sinf"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skip"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skipV"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "span"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanEnd"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanStart"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "verbosely"},
                                 Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_label", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                                 Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_moduleInfo", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                                 Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_prefix", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                                 Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_verbosity", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                                 Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo", typeName = Ident () "RenderInfo"},
                                 Data {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo"}])
                  (declaredSymbols (env', _module i))

test2 :: Test
test2 =
  TestCase $ do (i, env') <- loadMyModule "FGL"
                assertEqual "exported 1"
                  (Set.fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "components"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "context"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "delEdge"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "efilter"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "evalGraph"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "evalGraphT"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "insEdge"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "insNode"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labEdges"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labEdges'"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labEdgesM"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labNode"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labNode'"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labNodes"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labNodes'"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "mkGraph"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "mkGraphM"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "mkNode"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "out"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "reachable"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "runGraph"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "runGraphT"},
                                 Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "tests"}])
                  (exportedSymbols' (env', _module i))

test3 :: Test
test3 =
  TestCase $ do (i, env') <- loadMyModule "FGL"
                assertEqual "imported 1"
                  (Set.fromList
                   [Value {symbolModule = ModuleName () "Control.Lens.Setter", symbolName = Ident () "over"},
                    Value {symbolModule = ModuleName () "Control.Monad.Trans.State.Lazy", symbolName = Ident () "evalState"},
                    Value {symbolModule = ModuleName () "Control.Monad.Trans.State.Lazy", symbolName = Ident () "evalStateT"},
                    Value {symbolModule = ModuleName () "Control.Monad.Trans.State.Lazy", symbolName = Ident () "runState"},
                    Value {symbolModule = ModuleName () "Control.Monad.Trans.State.Lazy", symbolName = Ident () "runStateT"},
                    Value {symbolModule = ModuleName () "Data.Foldable", symbolName = Ident () "foldrM"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "context"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "delLEdge"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "gelem"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "insEdge"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "insNode"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "labNode'"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "lsuc'"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "nodes"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "out"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.NodeMap", symbolName = Ident () "fromGraph"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.NodeMap", symbolName = Ident () "mkNode"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.NodeMap", symbolName = Ident () "new"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Query.DFS", symbolName = Ident () "components"},
                    Value {symbolModule = ModuleName () "Data.Graph.Inductive.Query.DFS", symbolName = Ident () "reachable"},
                    Value {symbolModule = ModuleName () "Data.Map.Base", symbolName = Ident () "filter"},
                    Value {symbolModule = ModuleName () "Data.Map.Base", symbolName = Ident () "fromList"},
                    Value {symbolModule = ModuleName () "Data.Map.Base", symbolName = Ident () "insert"},
                    Value {symbolModule = ModuleName () "Data.Set.Base", symbolName = Ident () "fromList"},
                    Value {symbolModule = ModuleName () "Data.Set.Base", symbolName = Ident () "null"},
                    Value {symbolModule = ModuleName () "Data.Tuple", symbolName = Ident () "swap"},
                    Value {symbolModule = ModuleName () "Test.HUnit.Lang", symbolName = Ident () "assertEqual"},
                    Method {symbolModule = ModuleName () "Control.Lens.Tuple", symbolName = Ident () "_1", className = Ident () "Field1"},
                    Method {symbolModule = ModuleName () "Control.Monad.State.Class", symbolName = Ident () "get", className = Ident () "MonadState"},
                    Method {symbolModule = ModuleName () "Control.Monad.State.Class", symbolName = Ident () "put", className = Ident () "MonadState"},
                    Method {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "empty", className = Ident () "Graph"},
                    Method {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "labEdges", className = Ident () "Graph"},
                    Method {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "labNodes", className = Ident () "Graph"},
                    Constructor {symbolModule = ModuleName () "Test.HUnit.Base", symbolName = Ident () "TestCase", typeName = Ident () "Test"},
                    Constructor {symbolModule = ModuleName () "Test.HUnit.Base", symbolName = Ident () "TestList", typeName = Ident () "Test"},
                    Type {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "LNode"},
                    Type {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "Node"},
                    Data {symbolModule = ModuleName () "Data.Graph.Inductive.NodeMap", symbolName = Ident () "NodeMap"},
                    Data {symbolModule = ModuleName () "Data.Map.Base", symbolName = Ident () "Map"},
                    Data {symbolModule = ModuleName () "Data.Set.Base", symbolName = Ident () "Set"},
                    Data {symbolModule = ModuleName () "Test.HUnit.Base", symbolName = Ident () "Test"},
                    NewType {symbolModule = ModuleName () "Control.Monad.Trans.State.Lazy", symbolName = Ident () "StateT"},
                    NewType {symbolModule = ModuleName () "Data.Functor.Identity", symbolName = Ident () "Identity"},
                    NewType {symbolModule = ModuleName () "Data.Graph.Inductive.PatriciaTree", symbolName = Ident () "Gr"},
                    Class {symbolModule = ModuleName () "Control.Monad.State.Class", symbolName = Ident () "MonadState"},
                    Class {symbolModule = ModuleName () "Data.Graph.Inductive.Graph", symbolName = Ident () "DynGraph"}])
                  (importedSymbols (env', _module i))

-- | Symbols declared but not exported are local
test4 :: Test
test4 =
  TestCase $ do (i, env') <- loadMyModule "FGL"
                assertEqual "local 1"
                  (fromList
                   [Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "edgesFromGraph"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "evalGraph'"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "foldrM'"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labEdgesM'"}])
                  (Set.difference (declaredSymbols (env', _module i)) (exportedSymbols' (env', _module i)))

test5 :: Test
test5 =
  TestCase $ do (i, env') <- loadMyModule "Render"
                assertEqual "referenced 1"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "initRenderInfo"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep"},
                             -- To discover that keep', keep'', keepV, skipV, etc are defined but
                             -- not used we need to build a graph of the "referenced" relation on
                             -- declarations and exports, and any declaration that can't reach an
                             -- export is unused.
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep'"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep''"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keepV"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "labelled"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "prefixed"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "renderModule"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "sinf"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skip"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skipV"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "span"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanEnd"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanStart"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "verbosely"},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_label", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_moduleInfo", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_prefix", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_verbosity", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo", typeName = Ident () "RenderInfo"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo"}])
                  (Set.filter (\ x -> symbolModule x == ModuleName () "Language.Haskell.Modules.Render")
                    (referencedSymbols env' (_module i)))

test5c :: Test
test5c =
  TestCase $ do (i, env') <- loadMyModule "Info"
                assertEqual "declared 2"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "moduleGlobals"},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_module", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleComments", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_modulePath", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleSpan", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleText", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo", typeName = Ident () "ModuleInfo"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ImportSpecWithDecl"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo"}])
                  (declaredSymbols (env', _module i))

test5d :: Test
test5d =
  TestCase $ do (i, env') <- loadMyModule "Info"
                assertEqual "referenced 3"
                  (fromList [Qual () (ModuleName () "Global") (Ident () "Table"),
                             UnQual () (Ident () "Comment"),
                             UnQual () (Ident () "Data"),
                             UnQual () (Ident () "Environment"),
                             UnQual () (Ident () "FilePath"),
                             UnQual () (Ident () "Functor"),
                             UnQual () (Ident () "ImportDecl"),
                             UnQual () (Ident () "ImportSpec"),
                             UnQual () (Ident () "ImportSpecWithDecl"),
                             UnQual () (Ident () "Maybe"),
                             UnQual () (Ident () "Module"),
                             UnQual () (Ident () "ModuleInfo"),
                             UnQual () (Ident () "Show"),
                             UnQual () (Ident () "SrcSpanInfo"),
                             UnQual () (Ident () "String"),
                             UnQual () (Ident () "Table"),
                             UnQual () (Ident () "Typeable"),
                             UnQual () (Ident () "_module"),
                             UnQual () (Ident () "_moduleComments"),
                             UnQual () (Ident () "_modulePath"),
                             UnQual () (Ident () "_moduleSpan"),
                             UnQual () (Ident () "_moduleText"),
                             UnQual () (Ident () "deriveLift"),
                             UnQual () (Ident () "dropAnn"),
                             UnQual () (Ident () "env"),
                             UnQual () (Ident () "importTable"),
                             UnQual () (Ident () "l"),
                             UnQual () (Ident () "m"),
                             UnQual () (Ident () "moduleGlobals"),
                             UnQual () (Ident () "moduleTable")])
                  (Set.map dropAnn (referencedNames (_module i)))

test5b :: Test
test5b =
  TestCase $ do (i, env') <- loadMyModule "Info"
                assertEqual "referenced 2"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "moduleGlobals"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.Imports", symbolName = Ident () "importTable"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.ModuleSymbols", symbolName = Ident () "moduleTable"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.SyntaxUtils", symbolName = Ident () "dropAnn"},
                             Value {symbolModule = ModuleName () "Language.Haskell.TH.Lift", symbolName = Ident () "deriveLift"},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_module", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleComments", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_modulePath", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleSpan", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleText", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Exts.Comments", symbolName = Ident () "Comment", typeName = Ident () "Comment"},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo", typeName = Ident () "ModuleInfo"},
                             Type {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "String"},
                             Type {symbolModule = ModuleName () "GHC.IO", symbolName = Ident () "FilePath"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ImportSpecWithDecl"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Names.GlobalSymbolTable", symbolName = Ident () "Table"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Names.Types", symbolName = Ident () "Environment"},
                             Data {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "Maybe"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Comments", symbolName = Ident () "Comment"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.SrcLoc", symbolName = Ident () "SrcSpanInfo"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "ImportDecl"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "ImportSpec"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "Module"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo"},
                             Class {symbolModule = ModuleName () "Data.Data", symbolName = Ident () "Data"},
                             Class {symbolModule = ModuleName () "Data.Typeable.Internal", symbolName = Ident () "Typeable"},
                             Class {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "Functor"},
                             Class {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "Show"}])
                  (referencedSymbols env' (_module i))

test5e :: Test
test5e =
  TestCase $ do (i, env') <- loadMyModule "Info"
                let m = _module i
                    m' = dropAnn m
                assertEqual "declared 2"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "moduleGlobals"},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_module", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleComments", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_modulePath", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleSpan", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "_moduleText", typeName = Ident () "ModuleInfo", constructors = [Ident () "ModuleInfo"]},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo", typeName = Ident () "ModuleInfo"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ImportSpecWithDecl"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Modules.Info", symbolName = Ident () "ModuleInfo"}])
                  (Set.fromList (exportedSymbols (moduleTable (importTable env' m) m') m))

test5f :: Test
test5f =
  TestCase $ do (i, env') <- loadMyModule "Info"
                assertEqual "imported 2"
                  (fromList [Value {symbolModule = ModuleName () "Data.OldList", symbolName = Ident () "nub"}])
                  (importedSymbols (env', _module i))

test5g :: Test
test5g =
  TestCase $ do (i, env') <- loadMyModule "Info"
                let m = _module i
                    m' = dropAnn m
                assertEqual "redundant 1"
                  (fromList [Value {symbolModule = ModuleName () "Data.OldList", symbolName = Ident () "nub"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.Imports", symbolName = Ident () "importTable"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.ModuleSymbols", symbolName = Ident () "moduleTable"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Names.SyntaxUtils", symbolName = Ident () "dropAnn"},
                             Value {symbolModule = ModuleName () "Language.Haskell.TH.Lift", symbolName = Ident () "deriveLift"},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Exts.Comments", symbolName = Ident () "Comment", typeName = Ident () "Comment"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Names.GlobalSymbolTable", symbolName = Ident () "Table"},
                             Type {symbolModule = ModuleName () "Language.Haskell.Names.Types", symbolName = Ident () "Environment"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Comments", symbolName = Ident () "Comment"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.SrcLoc", symbolName = Ident () "SrcSpanInfo"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "ImportDecl"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "ImportSpec"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Exts.Syntax", symbolName = Ident () "Module"},
                             Class {symbolModule = ModuleName () "Data.Data", symbolName = Ident () "Data"},
                             Class {symbolModule = ModuleName () "Data.Typeable.Internal", symbolName = Ident () "Typeable"}])
                  (Set.difference
                     (importedSymbols (env', _module i))
                     (Set.union
                       (referencedSymbols env' (_module i))
                       (Set.fromList (exportedSymbols (moduleTable (importTable env' m) m') m))))

test6 :: Test
test6 =
  TestCase $ do (i, env') <- loadMyModule "Render"
                assertEqual "defined but not used 1"
                  (fromList []) -- need a better example
                  (Set.difference (declaredSymbols (env', _module i)) (referencedSymbols env' (_module i)))

demo1 = do
  (i, env) <- loadMyModule "FGL"
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule env components renderModule i) [1..])

-- | Pull out context and everything that uses it: context, labNode
demo3 = do
  (i, env) <- loadMyModule "FGL"
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s)
        (zip (withDecomposedModule env (bisect (\d -> any (testSymbolString (== "Language.Haskell.Modules.FGL.context")) (Set.unions (fmap (\d -> declaredSymbols (env2, _module i, d)) (unDecs d))))) renderModule i) [1..])

demo5 = do
  (i, env) <- loadMyModule "Info"
  let p = cleanImports env (_module i)
      -- m' = cleanImports env (_module i)
      s = renderModule i (const True) (const True) p
  putStrLn s

demo5b = do
  (i, env) <- loadMyModule "Reify"
  let p = cleanImports env (_module i)
      -- m' = cleanImports env (_module i)
      s = renderModule i (const True) (const True) p
  putStrLn s

demo5c = do
  (i, env) <- loadMyModule "Environment"
  let p = cleanImports env (_module i)
      -- m' = cleanImports env (_module i)
      s = renderModule i (const True) (const True) p
  putStrLn s

testSymbolString p s = p (ppSymbol s)
