{-# LANGUAGE LambdaCase #-}

module Day9 where

import Day9.Types

import Control.Lens (use, (%=), (.=), imap, view)
import Control.Monad (replicateM_, forM_, when)
import Control.Monad.State (execState, modify, get, put)
import Data.Char (isDigit)
import Data.List.Split (split, dropBlanks, dropDelims, whenElt)
import Text.Printf (printf)

import Debug.Trace

($$) :: (a -> a -> b) -> [a] -> b
f $$ (x : y : _) = f x y

parse :: String -> (Int, Int)
parse s = (,) $$ (read <$> split splitAtInts s)
  where splitAtInts = dropBlanks . dropDelims $ whenElt (not . isDigit)

zipperClockwise :: Zipper a -> Zipper a
zipperClockwise = \case
  Zipper [] [] -> Zipper [] []
  Zipper (x : xs) [] -> error "Zipper invariant broken"
  Zipper xs [y] -> Zipper [] (reverse (y : xs))
  Zipper xs (y : ys) -> Zipper (y : xs) ys

zipperCounter :: Zipper a -> Zipper a
zipperCounter = \case
  Zipper [] ys -> case reverse ys of
    [] -> Zipper [] []
    (y : ys) -> Zipper ys [y]
  Zipper (x : xs) ys -> Zipper xs (x : ys)

zipperPlace :: a -> Zipper a -> Zipper a
zipperPlace y (Zipper xs ys) = Zipper xs (y : ys)

zipperRemove :: (Show a) => Zipper a -> (a, Zipper a)
zipperRemove (Zipper xs (y : ys)) = (y, Zipper xs ys)
zipperRemove z = error $ "Cannot remove from: " ++ show z

clockwise :: Int -> Game ()
clockwise = flip replicateM_ (zipper %= zipperClockwise)

counter :: Int -> Game ()
counter = flip replicateM_ (zipper %= zipperCounter)

place :: Int -> Game ()
place y = zipper %= zipperPlace y

remove :: Game Int
remove = do { (y, z) <- zipperRemove <$> use zipper; zipper .= z; pure y }

addScore :: Int -> Int -> Game ()
addScore p amt = scores %= imap (\i x -> if i == p then x + amt else x)

marbles :: Int -> Int -> Game ()
marbles nPlayers nMarbles = do
  let turns = zip (cycle [1..nPlayers]) [1..nMarbles]
  scores .= replicate nPlayers 0
  place 0
  forM_ turns $ \(p, m) -> do
    when (m `mod` 23 /= 0) $ do
      clockwise 2
      place m
    when (m `mod` 23 == 0) $ do  -- else
      counter 7
      m' <- remove
      addScore p (m + m')

playGame :: Game () -> [Int]
playGame = view scores . flip execState (GameState (Zipper [] []) [])

run :: IO ()
run = do
  (nP, nM) <- parse <$> readFile "data/Day9.txt"
  printf "Part 1: %d\n" . maximum . playGame $ marbles nP nM
  printf "Part 2: %d\n" . maximum . playGame $ marbles nP (nM * 100)
