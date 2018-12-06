module Day5 where

import Data.Char (isSpace, toLower)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Printf (printf)


type Unit = Char
type Polymer = String

annihilate :: Unit -> Unit -> Bool
annihilate x y = x /= y && toLower x == toLower y

react :: Polymer -> Polymer
react = go []
  where
    go (x : xs) (y : ys) | annihilate x y = go xs ys
    go xs (y : ys) = go (y : xs) ys
    go xs [] = reverse xs

findSmallestPolymer :: Polymer -> Polymer
findSmallestPolymer p =
  minimumBy (comparing length) . fmap (react . removeUnit p) $ ['a'..'z']
  where removeUnit p u = filter ((/= u) . toLower) p

run :: IO ()
run = do
  polymer <- filter (not . isSpace) <$> readFile "data/Day5.txt"
  let stuck = react polymer
  printf "Part 1: %d\n" . length $ stuck
  printf "Part 2: %d\n" . length $ findSmallestPolymer stuck
