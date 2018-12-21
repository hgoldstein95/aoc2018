{-# LANGUAGE LambdaCase #-}

module Day13.Types where

import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Dir = North | South | East | West
  deriving (Show)

data Track = Vert | Horiz | Cross | FwdTurn | BckTurn
  deriving (Show)

type Loc = (Int, Int)

data Turn = Lt | Rt | Fwd
  deriving (Show)

data Cart
  = Crash Loc
  | Cart Loc Dir Turn
  deriving (Show)

isCrash :: Cart -> Bool
isCrash = \case { Crash _ -> True; _ -> False }

location :: Cart -> Loc
location = \case { Crash l -> l; Cart l _ _ -> l }

data Model = Model
  { carts :: [Cart]
  , track :: Map Loc Track
  } deriving (Show)

pretty :: Int -> Int -> Model -> String
pretty xmax ymax (Model carts track) =
  intercalate "\n" . fmap (fmap printPoint) $ points
  where
    points :: [[Loc]]
    points = [[(x, y) | x <- [0..xmax]] | y <- [0..ymax]]
    printPoint :: Loc -> Char
    printPoint p
      | p `elem` (location <$> carts) =
        case filter ((== p) . location) carts of
          (Crash _ : _) -> 'X'
          (Cart _ dir _ : _) -> case dir of
            North -> '^'
            East -> '>'
            South -> 'v'
            West -> '<'
      | p `Map.member` track =
        case track ! p of
          Vert -> '|'
          Horiz -> '-'
          Cross -> '+'
          FwdTurn -> '/'
          BckTurn -> '\\'
      | otherwise = ' '
