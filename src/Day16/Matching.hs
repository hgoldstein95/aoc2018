{- Code adapted from:
    https://mail.haskell.org/pipermail/haskell-cafe/2012-October/104059.html
-}

module Day16.Matching (findMatching) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

findMatching :: (Ord a, Ord b) => Map a [b] -> Map a b
findMatching fwd =
  Map.fromList .
  fmap swap .
  Map.assocs $
  opt (Map.keys fwd, []) fwd Map.empty

opt :: (Ord a, Ord b) => ([a], [a]) -> Map a [b] -> Map b a -> Map b a
opt (x:free,failed) fwd mat =
  either (flip (opt (free, x : failed)) mat) (opt (free ++ failed, []) fwd) $
  right fwd [] x
  where
     right rem path x =
       maybe (Left rem) (left $ Map.delete x rem) $ Map.lookup x rem
       where
         left rem [] = Left rem
         left rem (y : ys) =
           let path' = (x, y) : path
           in maybe
              (Right $ foldr (uncurry $ flip Map.insert) mat path')
              (either (`left` ys) Right . right rem path')
              (Map.lookup y mat)

opt ([],failed) fwd mat = mat
