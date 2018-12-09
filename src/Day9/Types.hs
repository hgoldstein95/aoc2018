{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Day9.Types where

import Control.Lens (makeLenses)
import Control.Monad.State.Strict (State, evalState)
import Data.Array (Array, listArray)
import Data.Maybe (fromJust)
import Data.Dequeue

type Marble = Int

type Circle = BankersDequeue

data GameState = GameState
  { _circle :: Circle Marble
  , _scores :: Array Int Int
  } deriving (Show)

type Game = State GameState

emptyCircle :: Circle a
emptyCircle = empty

circleClockwise :: Int -> Circle a -> Circle a
circleClockwise n dq = iterate circleOnce dq !! n
  where circleOnce (popFront -> Just (x, xs)) = pushBack xs x

circleCounter :: Int -> Circle a -> Circle a
circleCounter n dq = iterate circleOnce dq !! n
  where circleOnce (popBack -> Just (x, xs)) = pushFront xs x

circlePlace :: a -> Circle a -> Circle a
circlePlace = flip pushFront

circleRemove :: (Show a) => Circle a -> (a, Circle a)
circleRemove = fromJust . popFront

makeLenses ''GameState

playGame :: Game [Int] -> [Int]
playGame = flip evalState initState
  where initState = GameState emptyCircle (listArray (0, 0) [])
