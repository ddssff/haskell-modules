{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Utils where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data, listify, Typeable)
import Data.Map as Map (fromList, Map)
import Data.Set as Set (fromList, Set)
import qualified Data.Set as Set
import Debug.Show (V(V))
import Language.Haskell.TH.Syntax

flatten :: Ord a => Set (Set a) -> Set a
flatten = foldl Set.union mempty

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

-- | Generically find all values of type b in a value of type a
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

-- | Slightly modified lines function from Data.List.  It preserves
-- the presence or absence of a terminating newline by appending [""]
-- if string ends with a newline.  Thus, the corresponding unlines
-- function is intercalate "\n".
lines'                   :: String -> [String]
lines' ""                =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
lines' s                 =  cons (case break (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> [] -- no newline
                                                    _:""    -> [""]
                                                    _:s''   -> lines' s''))
  where
    cons ~(h, t)        =  h : t

-- Works when it works.
singleton :: Applicative m => a -> m a
singleton = pure

showName :: Name -> String
showName (Name (OccName s) (NameG VarName (PkgName p) (ModName m))) = m ++ "." ++ s ++ " from package " ++ p
showName n = show (V n)

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = Map.fromList

setFromList :: Ord a => [a] -> Set a
setFromList = Set.fromList
