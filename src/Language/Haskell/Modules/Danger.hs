-- | Symbols that are dangerous to reify.

{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Danger
    ( NamePattern(nameMatch)
    , dangerous
    , reify'
    , reify''
    ) where

import Control.Monad (msum, when)
import Control.Monad.Catch
import Data.List (isPrefixOf)
import GHC.IO.Exception
import Language.Haskell.Modules.Utils (showName)
import Language.Haskell.TH (lookupValueName, Q, reify, runIO, runQ, Type(TupleT))
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax as TH
    (Info(..), lift, ModName(..), Name(..), NameFlavour(..), NameSpace(..), OccName(..), PkgName(..))
import System.IO (hPutStrLn, stderr)

-- | Class of ways we can select a (dangerous to reify) 'Name' and
-- return the corresponding 'Info'.
class NamePattern a where
    nameMatch :: a -> Name -> Maybe Info

instance NamePattern Name where
    nameMatch name1 name2 | name1 == name2 = Just (VarI name2 (TupleT 0) Nothing)
    nameMatch _ _ = Nothing

instance NamePattern (String, String) where
    nameMatch (mname1, sname1) name@(Name (OccName sname2) (NameG _ _ (ModName mname2)))
        | mname1 == mname2 && sname1 == sname2 = Just (VarI name (TupleT 0) Nothing)
    nameMatch _ _ = Nothing

instance NamePattern (String, String, String) where
    nameMatch (pname1, mname1, sname1) name@(Name (OccName sname2) (NameG _ (PkgName pname2) (ModName mname2)))
        | mname1 == mname2 && sname1 == sname2 && isPrefixOf (pname1 ++ "-") pname2 =
            Just (VarI name (TupleT 0) Nothing)
    nameMatch _ _ = Nothing

-- avoid this one
instance NamePattern String where
    nameMatch sname1 name@(Name (OccName sname2) (NameG _ _ _))
        | sname1 == sname2 = Just (VarI name (TupleT 0) Nothing)
    nameMatch _ _ = Nothing

-- | Special case for functions in the prelude that template haskell
-- can't handle.
dangerous :: Name -> Maybe Info
dangerous name =
    msum [ nameMatch ($(lookupValueName "error" >>= \(Just x) -> lift x) :: Name) name
         , nameMatch (Name (OccName "undefined") (NameG VarName (PkgName "base") (ModName "GHC.Err"))) name
         , nameMatch ("Control.Lens.Fold", "^?!") name
         , nameMatch ("Control.Lens.Fold", "^@?!") name
         , nameMatch ("Control.Lens.Fold", "foldl1Of") name
         , nameMatch ("Control.Lens.Fold", "foldl1Of'") name
         , nameMatch ("Control.Lens.Fold", "foldr1Of") name
         , nameMatch ("Control.Lens.Fold", "foldr1Of'") name
         , nameMatch ("Control.Lens.Traversal", "singular") name
         , nameMatch ("Control.Lens.Traversal", "unsafeSingular") name
         , nameMatch ("GHC.IO.Exception", "assertError") name
         , nameMatch ("Test.HUnit.Base", "Assertable") name
         , nameMatch ("Test.HUnit.Base", "assert") name
         , nameMatch ("Test.HUnit.Base", "assertBool") name
         , nameMatch ("Test.HUnit.Base", "assertString") name
         , nameMatch ("Test.HUnit.Lang", "assertEqual") name
         , nameMatch ("Test.HUnit.Lang", "assertFailure") name
         , nameMatch ("Test.HUnit.Base", "ListAssertable") name
         , nameMatch ("Test.HUnit.Base", "listAssert") name
         , nameMatch ("Test.HUnit.Base", "Testable") name
         , nameMatch ("Test.HUnit.Base", "test") name
         , nameMatch ("Test.HUnit.Base", "@=?") name
         , nameMatch ("Test.HUnit.Base", "@?=") name
         , nameMatch ("Test.HUnit.Base", "@?") name
         , nameMatch ("Test.HUnit.Base", "~=?") name
         , nameMatch ("Test.HUnit.Base", "~?") name
         , nameMatch ("Test.HUnit.Base", "~?=") name
         , nameMatch ("Test.HUnit.Base", "~:") name]

-- | Reify if special case function fails.
reify' :: Int -> (Name -> Maybe Info) -> Name -> Q Info
reify' verbosity danger name =
    maybe (do when (verbosity > 0)
                (runIO (hPutStrLn stderr ("reify " ++ showName name ++ " special=" ++ show (danger name))))
              reify name)
          return
          (danger name)

-- | Unfortunately this always gives the dreaded "Can't do `reify' in the IO monad"
reify'' :: (Name -> Maybe Info) -> Name -> Q Info
reify'' danger name =
  runIO (catchIf
           (\e -> ioe_type e == UserError &&
                  ioe_description e == "Template Haskell failure")
           (runQ $ reify' 0 danger name)
           (\e  -> throwM (e {ioe_description = "Failure reifying " ++ showName name ++ ": " ++ ioe_description e})))
