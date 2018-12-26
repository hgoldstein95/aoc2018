{-# LANGUAGE LambdaCase #-}

module Day18 where

import Control.Monad (zipWithM_)
import Data.Array (Ix, Array)
import qualified Data.Array as Array
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

type Landscape = Array (Int, Int) Char

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

(!?) :: (Ix i) => Array i a -> i -> Maybe a
arr !? x =
  if Array.inRange (Array.bounds arr) x
  then Just $ arr Array.! x
  else Nothing

arrMap :: (Ix i) => (i -> a -> b) -> Array i a -> Array i b
arrMap f arr = Array.array (Array.bounds arr)
  [ (i, f i (arr Array.! i))
  | i <- Array.range (Array.bounds arr)
  ]

parseGrid :: String -> Landscape
parseGrid s = Array.array ((0, 0), (xmax - 1, ymax - 1))
  [((x, y), c) | (y, l) <- zip [0, 1..] ls , (x, c) <- zip [0, 1..] l]
  where
    ls = lines s
    xmax = maximum . fmap length $ ls
    ymax = length ls

neighbors :: Landscape -> (Int, Int) -> (Int, Int, Int)
neighbors land a@(x, y) =
  (count (== '.') acres, count (== '|') acres, count (== '#') acres)
  where
    acres = mapMaybe (land !?)
      [(u, v) | u <- [x - 1..x + 1] , v <- [y - 1..y + 1] , (u, v) /= (x, y)]

update :: Landscape -> (Int, Int) -> Char -> Char
update land a = \case
  '.' -> if trees >= 3 then '|' else '.'
  '|' -> if lumber >= 3 then '#' else '|'
  '#' -> if lumber > 0 && trees > 0 then '#' else '.'
  where (open, trees, lumber) = neighbors land a

step :: Landscape -> Landscape
step land = arrMap (update land) land

value :: Landscape -> Int
value land = count (== '|') acres * count (== '#') acres
  where acres = Array.elems land

run :: IO ()
run = do
  land <- parseGrid <$> readFile "data/Day18.txt"
  printf "Part 1: %d\n" . value . (!! 10) $ iterate step land
  -- Part 2:
  --   1. Run code below.
  --   2. Find repeating sequence, compute
  --      (1000000000 - endOfSeq) `mod` lenOfSeq
  --   3. Take that offset in the sequence.
  -- zipWithM_ (printf "%d\t%d\n") [0 :: Int, 1..] (value <$> iterate step land)
