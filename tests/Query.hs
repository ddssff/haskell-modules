{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Query where

import Control.Monad.State (evalStateT, runStateT)
import Data.Default (def)
import Data.Graph.Inductive (grev)
import Data.Set as Set (difference, empty, filter, fromList, union, unions)
import Language.Haskell.Exts (Name(Ident, Symbol))
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Language.Haskell.Names (ppSymbol, Symbol(..))
import Language.Haskell.TH.Instances ()
import Prelude hiding (lookup)
import Language.Haskell.Modules.FGL (components)
import Language.Haskell.Modules.Info (moduleGlobals, ModuleInfo(..))
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Modules.Parse (parseAndAnnotateModules)
import Language.Haskell.Modules.Query (declaredSymbols, exportedSymbols, importedSymbols, referencedSymbols)
import Language.Haskell.Modules.Render (renderModule)
import Language.Haskell.Modules.Split (bisect, declares, DeclGroup(unDecs), withDecomposedModule)
import System.IO (readFile, writeFile)
import Test.HUnit hiding (path)

import Environment (env2)

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6]

test1 :: Test
test1 =
  TestCase $ do let path = "src/Language/Haskell/Modules/Render.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
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
                  (declaredSymbols (env', i))

test2 :: Test
test2 =
  TestCase $ do let path = "src/Language/Haskell/Modules/FGL.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
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
                  (exportedSymbols (env', i))

test3 :: Test
test3 =
  TestCase $ do let path = "src/Language/Haskell/Modules/FGL.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
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
                  (importedSymbols (env', i))

-- | Symbols declared but not exported are local
test4 :: Test
test4 =
  TestCase $ do let path = "src/Language/Haskell/Modules/FGL.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
                assertEqual "local 1"
                  (fromList
                   [Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "edgesFromGraph"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "evalGraph'"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "foldrM'"},
                    Value {symbolModule = ModuleName () "Language.Haskell.Modules.FGL", symbolName = Ident () "labEdgesM'"}])
                  (Set.difference (declaredSymbols (env', i)) (exportedSymbols (env', i)))

test5 :: Test
test5 =
  TestCase $ do let path = "src/Language/Haskell/Modules/Render.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
                assertEqual "referenced 1"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "initRenderInfo"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "labelled"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "renderModule"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "sinf"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skip"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "span"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanEnd"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "spanStart"},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_verbosity", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Constructor {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo", typeName = Ident () "RenderInfo"},
                             Data {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "RenderInfo"}])
                  (Set.filter (\ x -> symbolModule x == ModuleName () "Language.Haskell.Modules.Render")
                    (referencedSymbols (env', moduleGlobals env' (_module i), _module i)))

test6 :: Test
test6 =
  TestCase $ do let path = "src/Language/Haskell/Modules/Render.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
                assertEqual "defined but not used 1"
                  (fromList [Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep'"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keep''"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "keepV"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "prefixed"},
                             -- Used by a function which is itself unused.  Needs a graph.
                             -- Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "labelled"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "skipV"},
                             Value {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "verbosely"},
                             -- These three would be used by the expanded template haskell code
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_label", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_moduleInfo", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]},
                             Selector {symbolModule = ModuleName () "Language.Haskell.Modules.Render", symbolName = Ident () "_prefix", typeName = Ident () "RenderInfo", constructors = [Ident () "RenderInfo"]}
                            ])
                  (Set.difference (declaredSymbols (env', i)) (referencedSymbols (env', i)))

test7 :: Test
test7 =
  TestCase $ do let path = "src/Language/Haskell/Modules/Info.hs"
                ([i], env') <- readFile path >>= \text -> runStateT (parseAndAnnotateModules def [(path, text)]) env2
                assertEqual "redundant imports 1"
                  Set.empty
                  (Set.difference
                     (importedSymbols (env', i))
                     (Set.union
                        (referencedSymbols (env', i))
                        (exportedSymbols (env', i))))

demo1 = do
  [i] <- let path = "src/Langauge/Haskell/Modules/FGL.hs" in readFile path >>= \text -> evalStateT (parseAndAnnotateModules def [(path, text)]) env2
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule env2 components renderModule i) [1..])

demo2 = do
  [i] <- let path = "src/Langauge/Haskell/Modules/FGL.hs" in readFile path >>= \text -> evalStateT (parseAndAnnotateModules def [(path, text)]) env2
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule env2 (components . grev) renderModule i) [1..])

-- | Pull out context and everything that uses it: context, labNode
demo3 = do
  [i] <- let path = "src/Langauge/Haskell/Modules/FGL.hs" in readFile path >>= \text -> evalStateT (parseAndAnnotateModules def [(path, text)]) env2
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule env2 (bisect (\d -> any (testSymbolString (== "Language.Haskell.Modules.FGL.context")) (Set.unions (fmap (declares env2 i) (unDecs d))))) renderModule i) [1..])

-- | Pull out context and everything it uses.
demo4 = do
  [i] <- let path = "src/Langauge/Haskell/Modules/FGL.hs" in readFile path >>= \text -> evalStateT (parseAndAnnotateModules def [(path, text)]) env2
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule env2 (bisect (\d -> any (testSymbolString (== "Language.Haskell.Modules.FGL.context")) (Set.unions (fmap (declares env2 i) (unDecs d)))) . grev) renderModule i) [1..])

testSymbolString p s = p (ppSymbol s)
