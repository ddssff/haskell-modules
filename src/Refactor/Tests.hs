module Refactor.Tests where

import Data.Default (def)
import Data.Graph.Inductive (grev)
import Data.Set as Set (fromList, unions)
import Language.Haskell.Exts (Decl, Name(Ident))
import Language.Haskell.Exts.ExactPrint (exactPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Exts.Syntax (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Names (annotate, ppSymbol, resolve, Scoped(..), Symbol(..))
import Refactor.FGL (components)
import Refactor.Filter (filterModule)
import Refactor.Info
import Refactor.Parse (parseAndAnnotateModules, unScope)
import Refactor.Render (renderModule)
import Refactor.Split (bisect, declares, DeclGroup(unDecs), withDecomposedModule)
import System.IO (readFile, writeFile)

test1 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule components renderModule i) [1..])

test2 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (components . grev) renderModule i) [1..])

-- | Pull out context and everything that uses it: context, labNode
test3 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (bisect (\d -> any (testSymbolString (== "Refactor.FGL.context")) (Set.unions (fmap (declares i) (unDecs d))))) renderModule i) [1..])

-- | Pull out context and everything it uses.
test4 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (bisect (\d -> any (testSymbolString (== "Refactor.FGL.context")) (Set.unions (fmap (declares i) (unDecs d)))) . grev) renderModule i) [1..])

testSymbolString p s = {-trace ("p " ++ show s ++ " -> " ++ show (p (ppSymbol s)))-} (p (ppSymbol s))
