{-# LANGUAGE LambdaCase #-}

module Day13 where

import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Map (Map, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Text.Printf (printf)

import Day13.Types

parseTrack :: Char -> Track
parseTrack = \case
  '|' -> Vert
  '^' -> Vert
  'v' -> Vert
  '-' -> Horiz
  '<' -> Horiz
  '>' -> Horiz
  '+' -> Cross
  '/' -> FwdTurn
  '\\' -> BckTurn

parseCart :: Loc -> Char -> Cart
parseCart l = \case
  '^' -> Cart l North Lt
  'v' -> Cart l South Lt
  '<' -> Cart l West Lt
  '>' -> Cart l East Lt

parseModel :: String -> Model
parseModel s = Model
  { carts = Map.elems . Map.mapWithKey parseCart .
             Map.filter (`elem` ['v', '^', '>', '<']) $ charMap
  , track = parseTrack <$> charMap
  }
  where charMap = Map.fromList
          [ ((x, y), c)
          | (y, line) <- zip [0, 1..] . lines $ s
          , (x, c) <- zip [0, 1..] line
          , not $ isSpace c
          ]

move :: Loc -> Dir -> Loc
move (x, y) = \case
  North -> (x, y - 1)
  South -> (x, y + 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

applyTurn :: Turn -> Dir -> Dir
applyTurn Lt = \case
  North -> West
  West -> South
  South -> East
  East -> North
applyTurn Rt = \case
  North -> East
  East -> South
  South -> West
  West -> North
applyTurn Fwd = id

update :: Dir -> Turn -> Track -> (Dir, Turn)
update dir turn track = case (dir, track) of
  (_, Vert) -> (dir, turn)
  (_, Horiz) -> (dir, turn)

  (North, FwdTurn) -> (East, turn)
  (East, FwdTurn) -> (North, turn)
  (South, FwdTurn) -> (West, turn)
  (West, FwdTurn) -> (South, turn)

  (North, BckTurn) -> (West, turn)
  (East, BckTurn) -> (South, turn)
  (South, BckTurn) -> (East, turn)
  (West, BckTurn) -> (North, turn)

  (_, Cross) -> case turn of
    Lt -> (applyTurn Lt dir, Fwd)
    Fwd -> (applyTurn Fwd dir, Rt)
    Rt -> (applyTurn Rt dir, Lt)

step :: Map Loc Track -> [Cart] -> Cart -> [Cart]
step track carts cart
  | location cart `elem` (location <$> carts) =
      Crash (location cart) : filter ((/= location cart) . location) carts
  | otherwise = case cart of
      Crash l -> Crash l : carts
      Cart loc dir turn ->
        let nextLoc = move loc dir
            (nextDir, nextTurn) = update dir turn $ track ! nextLoc
        in if nextLoc `elem` (location <$> carts) then
          Crash nextLoc : filter ((/= nextLoc) . location) carts
        else
          Cart nextLoc nextDir nextTurn : carts

tick :: Model -> Model
tick (Model carts track) = Model carts' track
  where carts' = foldl (step track) [] . sortBy (comparing location) $ carts

crashes :: Model -> [Loc]
crashes (Model carts _) = fmap location . filter isCrash $ carts

clearCrashes :: Model -> Model
clearCrashes (Model carts track) = Model (filter (not . isCrash) carts) track

lastCart :: Model -> Maybe Cart
lastCart (Model [cart] _) = Just cart
lastCart _ = Nothing

run :: IO ()
run = do
  input <- readFile "data/Day13.txt"
  let xmax = maximum . fmap length . lines $ input
      ymax = length . lines $ input
      model = parseModel input
  uncurry (printf "Part 1: %d,%d\n") .
    head . head . dropWhile null . fmap crashes
    $ iterate tick model
  uncurry (printf "Part 2: %d,%d\n") .
    location . fromJust . head . dropWhile isNothing . fmap lastCart
    $ iterate (clearCrashes . tick) model
