{-# OPTIONS -Wall #-}

module Refactor.Utils where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data, listify, Typeable)
import Data.Set as Set (Set)
import qualified Data.Set as Set

flatten :: Ord a => Set (Set a) -> Set a
flatten = foldl Set.union mempty

uncurry4 :: (a -> b -> c -> d -> r) -> (a, b, c, d) -> r
uncurry4 f (a, b, c, d) = f a b c d

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds = fmap (\((a, b, c), d) -> (a, b, c, d)) (zip (zip3 as bs cs) ds)

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