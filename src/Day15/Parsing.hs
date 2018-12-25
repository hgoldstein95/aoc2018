{-# LANGUAGE TupleSections #-}

module Day15.Parsing where

import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace (traceShowId)

import Day15.Types

type CombatBuilder = State UnitID

freshID :: CombatBuilder UnitID
freshID = do
  x <- get
  put (x + 1)
  pure x

newUnit :: Loc -> Char -> CombatBuilder Unit
newUnit l c = do { u <- freshID; pure $ Unit u 200 3 (align c) l }
  where align c = case c of { 'E' -> Elf; 'G' -> Goblin }

newTile :: Loc -> Char -> CombatBuilder Tile
newTile _ '.' = pure Empty
newTile _ '#' = pure Rock
newTile l c = Entity <$> newUnit l c

newCombatState :: String -> CombatBuilder CombatState
newCombatState s = do
  let lns = lines s
      xmax = maximum . fmap length $ lns
      ymax = length lns
  cavern <- Map.fromList <$> sequence
    [ (At x y,) <$> newTile (At x y) c
    | (y, ln) <- zip [0, 1..] lns
    , (x, c) <- zip [0, 1..] ln
    ]
  pure $ CombatState cavern (xmax - 1, ymax - 1) 0 (-1) 0

initialState :: String -> CombatState
initialState s = evalState (newCombatState s) 0
