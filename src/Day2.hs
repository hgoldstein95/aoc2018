module Day2 where

import Control.Lens (over, both)
import Control.Monad (zipWithM)
import Data.List (tails, group, sort, find)
import Data.Maybe (fromJust, catMaybes)
import Text.Printf (printf)


type Box = String

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

computeChecksum :: [Box] -> Int
computeChecksum ids =
  count (\s -> 2 `elem` repeats s) ids * count (\s -> 3 `elem` repeats s) ids
  where repeats = fmap length . group . sort

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

findMatchingBoxes :: [Box] -> (Box, Box)
findMatchingBoxes ids = fromJust $ find (uncurry differByOne) (pairs ids)
  where differByOne l r = count id (zipWith (/=) l r) == 1

commonLetters :: Box -> Box -> String
commonLetters l r = catMaybes $ zipWith keepIfEqual l r
  where keepIfEqual x y = if x == y then Just x else Nothing

run :: IO ()
run = do
  ids <- lines <$> readFile "data/Day2.txt"
  printf "Part 1: %d\n" $ computeChecksum ids
  printf "Part 2: %s\n" . uncurry commonLetters $ findMatchingBoxes ids
