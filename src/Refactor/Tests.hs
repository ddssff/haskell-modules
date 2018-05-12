{-# LANGUAGE CPP #-}

module Refactor.Tests where

import Data.Default (def)
import Data.Function (on)
import Data.Graph.Inductive (grev)
import Data.List (sortBy)
import Data.Map as Map (lookup)
import Data.Set as Set (difference, fromList, toList, unions)
import Language.Haskell.Exts (Decl, Name(Ident))
import Language.Haskell.Exts.ExactPrint (exactPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Exts.Syntax (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import qualified Language.Haskell.Exts.Syntax as Exts
import Language.Haskell.Interpreter (runInterpreter, getModuleExports{-, InterpreterError-})
import qualified Language.Haskell.Interpreter as Hint (ModuleElem(..))
import Language.Haskell.Names (annotate, loadBase, ppSymbol, resolve, Scoped(..), Symbol(..))
import Language.Haskell.Names as Names -- (loadBase, Symbol(..))
import Language.Haskell.TH.Syntax (lift, runIO)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.Exts.Syntax (Name(..), ModuleName(..))
import qualified Language.Haskell.Exts.Syntax as Exts
import Language.Haskell.Interpreter (runInterpreter, getModuleExports{-, InterpreterError-})
import qualified Language.Haskell.Interpreter as Hint (ModuleElem(..))
import Language.Haskell.Names as Names -- (loadBase, Symbol(..))
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax (Dec(..), Q, lookupValueName, lookupTypeName, reify, reifyModule, runIO, Info(..), ModName(..), PkgName(..), Name(..), OccName(..), NameFlavour(..), NameSpace(..))
import qualified Language.Haskell.TH.Syntax as TH
import Prelude hiding (lookup)
import Refactor.FGL (components)
import Refactor.Filter (filterModule)
import Refactor.Info
import Refactor.Orphans
import Refactor.Parse (parseAndAnnotateModules, unScope)
import Refactor.Reify (moduleSymbols)
import Refactor.Render (renderModule)
import Refactor.Split (bisect, declares, DeclGroup(unDecs), withDecomposedModule)
import System.IO (readFile, writeFile)
import Test.HUnit

demo1 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule components renderModule i) [1..])

demo2 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (components . grev) renderModule i) [1..])

-- | Pull out context and everything that uses it: context, labNode
demo3 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (bisect (\d -> any (testSymbolString (== "Refactor.FGL.context")) (Set.unions (fmap (declares i) (unDecs d))))) renderModule i) [1..])

-- | Pull out context and everything it uses.
demo4 = do
  [i] <- let path = "src/Refactor/FGL.hs" in readFile path >>= \text -> parseAndAnnotateModules def [(path, text)]
  mapM_ (\(s, n) -> writeFile ("Tmp" ++ show n ++ ".hs") s) (zip (withDecomposedModule (bisect (\d -> any (testSymbolString (== "Refactor.FGL.context")) (Set.unions (fmap (declares i) (unDecs d)))) . grev) renderModule i) [1..])

testSymbolString p s = {-trace ("p " ++ show s ++ " -> " ++ show (p (ppSymbol s)))-} (p (ppSymbol s))

#if 0
prelude :: TH.Module
prelude = TH.Module (PkgName "base") (ModName "Prelude")
#endif
{-
load :: TH.Module () -> [[Symbol]]
load m@(Module (PkgName p) (ModName n)) =
    reifyModule m >>= \(ModuleInfo ms) -> (map (undefined ::
(() (Just (ModuleHead () name _warning _exports) h ps is ds) =
    reifyModule (TH.Module ()
-}

tests :: Test
tests = TestList [test1, test2, test3, test4]

test1 :: Test
test1 = TestCase (assertEqual "reifyModule Prelude"
                    ($(TH.reifyModule (TH.Module (TH.PkgName "base") (TH.ModName "Prelude")) >>= lift . show))
                    (show (TH.ModuleInfo
                             [TH.Module (PkgName "base") (ModName "Control.Monad"),
                              TH.Module (PkgName "base") (ModName "Data.Either"),
                              TH.Module (PkgName "base") (ModName "Data.Foldable"),
                              TH.Module (PkgName "base") (ModName "Data.Functor"),
                              TH.Module (PkgName "base") (ModName "Data.List"),
                              TH.Module (PkgName "base") (ModName "Data.Maybe"),
                              TH.Module (PkgName "base") (ModName "Data.Traversable"),
                              TH.Module (PkgName "base") (ModName "Data.Tuple"),
                              TH.Module (PkgName "base") (ModName "GHC.Base"),
                              TH.Module (PkgName "base") (ModName "GHC.Enum"),
                              TH.Module (PkgName "base") (ModName "GHC.Float"),
                              TH.Module (PkgName "base") (ModName "GHC.Num"),
                              TH.Module (PkgName "base") (ModName "GHC.Read"),
                              TH.Module (PkgName "base") (ModName "GHC.Real"),
                              TH.Module (PkgName "base") (ModName "GHC.Show"),
                              TH.Module (PkgName "base") (ModName "System.IO"),
                              TH.Module (PkgName "base") (ModName "System.IO.Error"),
                              TH.Module (PkgName "base") (ModName "Text.Read"),
                              TH.Module (PkgName "ghc-prim") (ModName "GHC.Classes"),
                              TH.Module (PkgName "ghc-prim") (ModName "GHC.Types")])))

test2 :: Test
test2 = TestCase (assertEqual "getModuleExports Prelude"
                    [Hint.Class "Foldable" ["foldMap","foldr","foldl","foldr1","foldl1","null","length","elem","maximum","minimum","sum","product"],
                     Hint.Class "Traversable" ["traverse","sequenceA","mapM","sequence"],
                     Hint.Class "Applicative" ["pure","(<*>)","(*>)","(<*)"],
                     Hint.Class "Functor" ["fmap","(<$)"],
                     Hint.Class "Monad" ["(>>=)","(>>)","return","fail"],
                     Hint.Class "Monoid" ["mempty","mappend","mconcat"],
                     Hint.Class "Bounded" ["minBound","maxBound"],
                     Hint.Class "Enum" ["succ","pred","toEnum","fromEnum","enumFrom","enumFromThen","enumFromTo","enumFromThenTo"],
                     Hint.Class "Floating" ["pi","exp","log","sqrt","(**)","logBase","sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","asinh","acosh","atanh"],
                     Hint.Class "RealFloat" ["floatRadix","floatDigits","floatRange","decodeFloat","encodeFloat","exponent","significand","scaleFloat","isNaN","isInfinite","isDenormalized","isNegativeZero","isIEEE","atan2"],
                     Hint.Class "Num" ["(+)","(-)","(*)","negate","abs","signum","fromInteger"],
                     Hint.Class "Read" ["readsPrec","readList"],
                     Hint.Class "Fractional" ["(/)","recip","fromRational"],
                     Hint.Class "Integral" ["quot","rem","div","mod","quotRem","divMod","toInteger"],
                     Hint.Class "Real" ["toRational"],
                     Hint.Class "RealFrac" ["properFraction","truncate","round","ceiling","floor"],
                     Hint.Class "Show" ["showsPrec","show","showList"],
                     Hint.Class "Eq" ["(==)","(/=)"],
                     Hint.Class "Ord" ["compare","(<)","(<=)","(>)","(>=)","max","min"],
                     Hint.Data "Either" ["Left","Right"], Hint.Data "String" [], Hint.Data "FilePath" [], Hint.Data "IOError" [], Hint.Data "Rational" [], Hint.Data "ShowS" [], Hint.Data "ReadS" [],
                     Hint.Data "IO" [], Hint.Data "Ordering" ["LT","EQ","GT"], Hint.Data "Integer" [], Hint.Data "Maybe" ["Nothing","Just"], Hint.Data "Bool" ["False","True"],
                     Hint.Data "Char" [], Hint.Data "Double" [], Hint.Data "Float" [], Hint.Data "Int" [], Hint.Data "Word" [],
                     Hint.Fun "either", Hint.Fun "all", Hint.Fun "and", Hint.Fun "any", Hint.Fun "concat", Hint.Fun "concatMap", Hint.Fun "mapM_", Hint.Fun "notElem",
                     Hint.Fun "or", Hint.Fun "sequence_", Hint.Fun "(<$>)", Hint.Fun "maybe", Hint.Fun "lines", Hint.Fun "unlines", Hint.Fun "unwords", Hint.Fun "words", Hint.Fun "curry",
                     Hint.Fun "fst", Hint.Fun "snd", Hint.Fun "uncurry", Hint.Fun "($!)", Hint.Fun "(++)", Hint.Fun "(.)", Hint.Fun "(=<<)", Hint.Fun "asTypeOf", Hint.Fun "const",
                     Hint.Fun "flip", Hint.Fun "id", Hint.Fun "map", Hint.Fun "otherwise", Hint.Fun "until", Hint.Fun "error", Hint.Fun "errorWithoutStackTrace", Hint.Fun "undefined",
                     Hint.Fun "ioError", Hint.Fun "userError", Hint.Fun "(!!)", Hint.Fun "break", Hint.Fun "cycle", Hint.Fun "drop", Hint.Fun "dropWhile", Hint.Fun "filter",
                     Hint.Fun "head", Hint.Fun "init", Hint.Fun "iterate", Hint.Fun "last", Hint.Fun "lookup", Hint.Fun "repeat", Hint.Fun "replicate", Hint.Fun "reverse",
                     Hint.Fun "scanl", Hint.Fun "scanl1", Hint.Fun "scanr", Hint.Fun "scanr1", Hint.Fun "span", Hint.Fun "splitAt", Hint.Fun "tail", Hint.Fun "take", Hint.Fun "takeWhile",
                     Hint.Fun "unzip", Hint.Fun "unzip3", Hint.Fun "zip", Hint.Fun "zip3", Hint.Fun "zipWith", Hint.Fun "zipWith3", Hint.Fun "subtract", Hint.Fun "lex",
                     Hint.Fun "readParen", Hint.Fun "(^)", Hint.Fun "(^^)", Hint.Fun "even", Hint.Fun "fromIntegral", Hint.Fun "gcd", Hint.Fun "lcm", Hint.Fun "odd",
                     Hint.Fun "realToFrac", Hint.Fun "showChar", Hint.Fun "showParen", Hint.Fun "showString", Hint.Fun "shows", Hint.Fun "appendFile", Hint.Fun "getChar",
                     Hint.Fun "getContents", Hint.Fun "getLine", Hint.Fun "interact", Hint.Fun "print", Hint.Fun "putChar", Hint.Fun "putStr", Hint.Fun "putStrLn",
                     Hint.Fun "readFile", Hint.Fun "readIO", Hint.Fun "readLn", Hint.Fun "writeFile", Hint.Fun "read", Hint.Fun "reads", Hint.Fun "(&&)", Hint.Fun "not",
                     Hint.Fun "(||)", Hint.Fun "($)", Hint.Fun "seq"]
                    $(runIO (runInterpreter (getModuleExports "Prelude")) >>= either (error . show) lift))

test3 :: Test
test3 = TestCase (assertEqual "loadBase Prelude"
                    (Just [Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Bool"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "False", typeName = Ident () "Bool"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "True", typeName = Ident () "Bool"},
                           Value {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "&&"},
                           Value {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "||"},
                           Value {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "not"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "otherwise"},
                           Data {symbolModule = ModuleName () "Data.Maybe", symbolName = Ident () "Maybe"},
                           Constructor {symbolModule = ModuleName () "Data.Maybe", symbolName = Ident () "Nothing", typeName = Ident () "Maybe"},
                           Constructor {symbolModule = ModuleName () "Data.Maybe", symbolName = Ident () "Just", typeName = Ident () "Maybe"},
                           Value {symbolModule = ModuleName () "Data.Maybe", symbolName = Ident () "maybe"},
                           Data {symbolModule = ModuleName () "Data.Either", symbolName = Ident () "Either"},
                           Constructor {symbolModule = ModuleName () "Data.Either", symbolName = Ident () "Left", typeName = Ident () "Either"},
                           Constructor {symbolModule = ModuleName () "Data.Either", symbolName = Ident () "Right", typeName = Ident () "Either"},
                           Value {symbolModule = ModuleName () "Data.Either", symbolName = Ident () "either"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "LT", typeName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "EQ", typeName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "GT", typeName = Ident () "Ordering"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Char"},
                           Type {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "String"},
                           Value {symbolModule = ModuleName () "Data.Tuple", symbolName = Ident () "fst"},
                           Value {symbolModule = ModuleName () "Data.Tuple", symbolName = Ident () "snd"},
                           Value {symbolModule = ModuleName () "Data.Tuple", symbolName = Ident () "curry"},
                           Value {symbolModule = ModuleName () "Data.Tuple", symbolName = Ident () "uncurry"},
                           Class {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "Eq"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "==", className = Ident () "Eq"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "/=", className = Ident () "Eq"},
                           Class {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "compare", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "<", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () "<=", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () ">=", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Symbol () ">", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "max", className = Ident () "Ord"},
                           Method {symbolModule = ModuleName () "GHC.Classes", symbolName = Ident () "min", className = Ident () "Ord"},
                           Class {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "succ", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "pred", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "toEnum", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "fromEnum", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "enumFrom", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "enumFromThen", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "enumFromTo", className = Ident () "Enum"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "enumFromThenTo", className = Ident () "Enum"},
                           Class {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "Bounded"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "minBound", className = Ident () "Bounded"},
                           Method {symbolModule = ModuleName () "GHC.Enum", symbolName = Ident () "maxBound", className = Ident () "Bounded"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Int"},
                           Data {symbolModule = ModuleName () "GHC.Integer.Type", symbolName = Ident () "Integer"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Float"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Double"},
                           Type {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "Rational"},
                           Class {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Symbol () "+", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Symbol () "-", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Symbol () "*", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "negate", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "abs", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "signum", className = Ident () "Num"},
                           Method {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "fromInteger", className = Ident () "Num"},
                           Class {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "Real"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "toRational", className = Ident () "Real"},
                           Class {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "quot", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "rem", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "div", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "mod", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "quotRem", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "divMod", className = Ident () "Integral"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "toInteger", className = Ident () "Integral"},
                           Class {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "Fractional"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Symbol () "/", className = Ident () "Fractional"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "recip", className = Ident () "Fractional"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "fromRational", className = Ident () "Fractional"},
                           Class {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "pi", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "exp", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "log", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "sqrt", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Symbol () "**", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "logBase", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "sin", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "cos", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "tan", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "asin", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "acos", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "atan", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "sinh", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "cosh", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "tanh", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "asinh", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "acosh", className = Ident () "Floating"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "atanh", className = Ident () "Floating"},
                           Class {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "RealFrac"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "properFraction", className = Ident () "RealFrac"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "truncate", className = Ident () "RealFrac"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "round", className = Ident () "RealFrac"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "ceiling", className = Ident () "RealFrac"},
                           Method {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "floor", className = Ident () "RealFrac"},
                           Class {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "floatRadix", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "floatDigits", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "floatRange", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "decodeFloat", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "encodeFloat", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "exponent", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "significand", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "scaleFloat", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "isNaN", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "isInfinite", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "isDenormalized", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "isIEEE", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "isNegativeZero", className = Ident () "RealFloat"},
                           Method {symbolModule = ModuleName () "GHC.Float", symbolName = Ident () "atan2", className = Ident () "RealFloat"},
                           Value {symbolModule = ModuleName () "GHC.Num", symbolName = Ident () "subtract"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "even"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "odd"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "gcd"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "lcm"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Symbol () "^"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Symbol () "^^"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "fromIntegral"},
                           Value {symbolModule = ModuleName () "GHC.Real", symbolName = Ident () "realToFrac"},
                           Class {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "Monad"},
                           Method {symbolModule = ModuleName () "GHC.Base", symbolName = Symbol () ">>=", className = Ident () "Monad"},
                           Method {symbolModule = ModuleName () "GHC.Base", symbolName = Symbol () ">>", className = Ident () "Monad"},
                           Method {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "return", className = Ident () "Monad"},
                           Method {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "fail", className = Ident () "Monad"},
                           Class {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "Functor"},
                           Method {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "fmap", className = Ident () "Functor"},
                           Value {symbolModule = ModuleName () "Control.Monad", symbolName = Ident () "mapM"},
                           Value {symbolModule = ModuleName () "Control.Monad", symbolName = Ident () "mapM_"},
                           Value {symbolModule = ModuleName () "Control.Monad", symbolName = Ident () "sequence"},
                           Value {symbolModule = ModuleName () "Control.Monad", symbolName = Ident () "sequence_"},
                           Value {symbolModule = ModuleName () "Control.Monad", symbolName = Symbol () "=<<"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "id"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "const"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Symbol () "."},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "flip"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Symbol () "$"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "until"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "asTypeOf"},
                           Value {symbolModule = ModuleName () "GHC.Err", symbolName = Ident () "error"},
                           Value {symbolModule = ModuleName () "GHC.Err", symbolName = Ident () "undefined"},
                           Value {symbolModule = ModuleName () "GHC.Prim", symbolName = Ident () "seq"},
                           Value {symbolModule = ModuleName () "Prelude", symbolName = Symbol () "$!"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "map"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Symbol () "++"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "filter"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "head"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "last"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "tail"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "init"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "null"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "length"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Symbol () "!!"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "reverse"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "foldl"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "foldl1"},
                           Value {symbolModule = ModuleName () "GHC.Base", symbolName = Ident () "foldr"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "foldr1"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "and"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "or"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "any"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "all"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "sum"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "product"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "concat"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "concatMap"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "maximum"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "minimum"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "scanl"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "scanl1"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "scanr"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "scanr1"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "iterate"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "repeat"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "replicate"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "cycle"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "take"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "drop"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "splitAt"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "takeWhile"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "dropWhile"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "span"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "break"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "elem"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "notElem"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "lookup"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "zip"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "zip3"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "zipWith"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "zipWith3"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "unzip"},
                           Value {symbolModule = ModuleName () "GHC.List", symbolName = Ident () "unzip3"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "lines"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "words"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "unlines"},
                           Value {symbolModule = ModuleName () "Data.List", symbolName = Ident () "unwords"},
                           Type {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "ShowS"},
                           Class {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "Show"},
                           Method {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "showsPrec", className = Ident () "Show"},
                           Method {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "showList", className = Ident () "Show"},
                           Method {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "show", className = Ident () "Show"},
                           Value {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "shows"},
                           Value {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "showChar"},
                           Value {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "showString"},
                           Value {symbolModule = ModuleName () "GHC.Show", symbolName = Ident () "showParen"},
                           Type {symbolModule = ModuleName () "Text.ParserCombinators.ReadP", symbolName = Ident () "ReadS"},
                           Class {symbolModule = ModuleName () "GHC.Read", symbolName = Ident () "Read"},
                           Method {symbolModule = ModuleName () "GHC.Read", symbolName = Ident () "readsPrec", className = Ident () "Read"},
                           Method {symbolModule = ModuleName () "GHC.Read", symbolName = Ident () "readList", className = Ident () "Read"},
                           Value {symbolModule = ModuleName () "Text.Read", symbolName = Ident () "reads"},
                           Value {symbolModule = ModuleName () "GHC.Read", symbolName = Ident () "readParen"},
                           Value {symbolModule = ModuleName () "Text.Read", symbolName = Ident () "read"},
                           Value {symbolModule = ModuleName () "GHC.Read", symbolName = Ident () "lex"},
                           NewType {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "IO"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "putChar"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "putStr"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "putStrLn"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "print"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "getChar"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "getLine"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "getContents"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "interact"},
                           Type {symbolModule = ModuleName () "GHC.IO", symbolName = Ident () "FilePath"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "readFile"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "writeFile"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "appendFile"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "readIO"},
                           Value {symbolModule = ModuleName () "System.IO", symbolName = Ident () "readLn"},
                           Type {symbolModule = ModuleName () "GHC.IO.Exception", symbolName = Ident () "IOError"},
                           Value {symbolModule = ModuleName () "GHC.IO.Exception", symbolName = Ident () "ioError"},
                           Value {symbolModule = ModuleName () "GHC.IO.Exception", symbolName = Ident () "userError"}])
                    (Map.lookup (Exts.ModuleName () "Prelude") $(runIO loadBase >>= lift)))

test4 :: Test
test4 = TestCase (assertEqual "loadBase GHC.Types"
                    (Just [Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Bool"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "False", typeName = Ident () "Bool"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "True", typeName = Ident () "Bool"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Char"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "C#", typeName = Ident () "Char"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Int"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "I#", typeName = Ident () "Int"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Word"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "W#", typeName = Ident () "Word"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Float"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "F#", typeName = Ident () "Float"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Double"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "D#", typeName = Ident () "Double"},
                           Data {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "EQ", typeName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "GT", typeName = Ident () "Ordering"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "LT", typeName = Ident () "Ordering"},
                           NewType {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "IO"},
                           Constructor {symbolModule = ModuleName () "GHC.Types", symbolName = Ident () "IO", typeName = Ident () "IO"}])
                    (Map.lookup (Exts.ModuleName () "GHC.Types") $(runIO loadBase >>= lift)))

{-
monad = ModuleInfo [Module (PkgName "base") (ModName "Data.Foldable"),
                    Module (PkgName "base") (ModName "Data.Functor"),
                    Module (PkgName "base") (ModName "Data.Traversable"),
                    Module (PkgName "base") (ModName "GHC.Base"),
                    Module (PkgName "base") (ModName "GHC.List"),
                    Module (PkgName "base") (ModName "GHC.Num"),
                    Module (PkgName "ghc-prim") (ModName "GHC.Classes"),
                    Module (PkgName "ghc-prim") (ModName "GHC.Types")]

base = ModuleInfo [Module (PkgName "base") (ModName "GHC.Err"),
                   Module (PkgName "base") (ModName "GHC.IO"),
                   Module (PkgName "ghc-prim") (ModName "GHC.CString"),
                   Module (PkgName "ghc-prim") (ModName "GHC.Classes"),
                   Module (PkgName "ghc-prim") (ModName "GHC.Magic"),
                   Module (PkgName "ghc-prim") (ModName "GHC.Prim"),
                   Module (PkgName "ghc-prim") (ModName "GHC.Tuple"),
                   Module (PkgName "ghc-prim") (ModName "GHC.Types"),
                   Module (PkgName "integer-gmp") (ModName "GHC.Integer")]
-}

test5 :: Test
test5 = TestCase (do let syms1 = Set.fromList $ maybe (error "test5") id $ Map.lookup (Exts.ModuleName () "Prelude") $(runIO loadBase >>= lift)
                         syms2 = Set.fromList $ $(moduleSymbols "Prelude")
                     assertEqual "loadBase vs moduleSymbols 1" (mempty, mempty)
                       (sortBy (compare `on` show) (Set.toList (Set.difference syms1 syms2)),
                        sortBy (compare `on` show) (Set.toList (Set.difference syms2 syms1))))
