module Day9 where

import Day9.Types

import Control.Lens (use, (%=), (.=), ix)
import Control.Monad (replicateM_, forM_, when)
import Control.Monad.State.Strict (modify, get, put)
import Data.Array (listArray, elems)
import Data.Char (isDigit)
import Data.List.Split (split, dropBlanks, dropDelims, whenElt)
import Text.Printf (printf)

import Debug.Trace

($$) :: (a -> a -> b) -> [a] -> b
f $$ (x : y : _) = f x y

parse :: String -> (Int, Int)
parse s = (,) $$ (read <$> split splitAtInts s)
  where splitAtInts = dropBlanks . dropDelims $ whenElt (not . isDigit)

clockwise :: Int -> Game ()
clockwise n = circle %= circleClockwise n

counter :: Int -> Game ()
counter n = circle %= circleCounter n

place :: Int -> Game ()
place y = circle %= circlePlace y

remove :: Game Int
remove = do { (y, z) <- circleRemove <$> use circle; circle .= z; pure y }

addScore :: Int -> Int -> Game ()
addScore p amt = scores.ix p %= (+ amt)

marbles :: Int -> Int -> Game [Int]
marbles nPlayers nMarbles = do
  let turns = zip (cycle [1..nPlayers]) [1..nMarbles]
  scores .= listArray (1, nPlayers) (replicate nPlayers 0)
  place 0
  forM_ turns $ \(p, m) -> do
    when (m `mod` 23 /= 0) $ do
      clockwise 2
      place m
    when (m `mod` 23 == 0) $ do  -- else
      counter 7
      m' <- remove
      addScore p (m + m')
  elems <$> use scores

run :: IO ()
run = do
  (nP, nM) <- parse <$> readFile "data/Day9.txt"
  printf "Part 1: %d\n" . maximum . playGame $ marbles nP nM
  printf "Part 2: %d\n" . maximum . playGame $ marbles nP (nM * 100)
