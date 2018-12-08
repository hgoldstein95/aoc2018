module Day8 where

import Day8.Types

import Control.Lens ((^?), element)
import Control.Monad (replicateM)
import Control.Monad.State (State, get, put, evalState)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)


next :: State [Int] Int
next = do { (x : xs) <- get; put xs; pure x }

mkNode :: State [Int] Tree
mkNode = do
  nc <- next
  nm <- next
  Node <$> replicateM nc mkNode <*> replicateM nm next

mkTree :: [Int] -> Tree
mkTree = evalState mkNode

sumMetadata :: Tree -> Int
sumMetadata (Node cs ms) = sum ms + sum (sumMetadata <$> cs)

value :: Tree -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum . mapMaybe (fmap value . idxChild . subtract 1) $ ms
  where idxChild = (cs ^?) . element

run :: IO ()
run = do
  tree <- mkTree . fmap read . words <$> readFile "data/Day8.txt"
  printf "Part 1: %d\n" . sumMetadata $ tree
  printf "Part 2: %d\n" . value $ tree
