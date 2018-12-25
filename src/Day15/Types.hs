{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Day15.Types where

import Control.Lens (makeLenses)
import Control.Monad.State.Strict (State, get, put)
import Data.List (intercalate, elemIndex, sortBy)
import Data.Maybe (mapMaybe)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord (comparing)

import Debug.Trace (traceShowId)

type UnitID = Int

data Loc = At Int Int
  deriving (Eq)

instance Show Loc where
  show (At x y) = show (x, y)

instance Ord Loc where
  (At x1 y1) `compare` (At x2 y2) = (y1, x1) `compare` (y2, x2)

data Alignment = Elf | Goblin
  deriving (Eq)

instance Show Alignment where
  show Elf = "E"
  show Goblin = "G"

data Unit = Unit
  { _uid :: UnitID
  , _health :: Int
  , _power :: Int
  , _alignment :: Alignment
  , _location :: Loc
  } deriving (Eq)

instance Show Unit where
  show (Unit _ h _ a l) = show a ++ "(" ++ show h ++ ") at " ++ show l

data Tile
  = Entity Unit
  | Empty
  | Rock
  deriving (Eq)

instance Show Tile where
  show (Entity (Unit _ _ _ a _)) = show a
  show Empty = "."
  show Rock = "#"

getUnit :: Tile -> Maybe Unit
getUnit (Entity u) = Just u
getUnit _ = Nothing

units :: Map Loc Tile -> [Unit]
units = mapMaybe getUnit . Map.elems

data CombatState = CombatState
  { _cavern :: Map Loc Tile
  , _bounds :: (Int, Int)
  , _roundNo :: Int
  , _fuel :: Int
  , _deadElves :: Int
  }

instance Show CombatState where
  show st = "Completed " ++ show r ++ " rounds...\n" ++
            cavern ++ "\n" ++
            intercalate "\n" (show <$> sortBy (comparing _location) (units c))
    where
      c = _cavern st
      (xmax, ymax) = _bounds st
      r = _roundNo st
      cavern = intercalate "\n" . fmap (concatMap (show . (c !))) $
        [ [ At x y | x <- [0..xmax] ]
        | y <- [0..ymax]
        ]

type Combat = State CombatState

dumpState :: Combat ()
dumpState = do
  s <- get
  put (traceShowId s)

makeLenses ''Unit
makeLenses ''CombatState
