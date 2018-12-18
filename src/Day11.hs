module Day11 where

import Control.Lens ((^?), element)
import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Array (array, Array, (!))
import Data.List (maximumBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Text.Printf (printf)

import qualified Data.Map as Map


type Cell = (Int, Int)
type Grid = Array (Int, Int) Int

powerLevel :: Int -> Cell -> Int
powerLevel serial (x, y) =
  let rackId = x + 10
  in subtract 5 . hundredths . (* rackId) . (+ serial) $ rackId * y
  where hundredths = maybe 0 (read . (: [])) . (^? element 2) . reverse . show

fuelCells :: [Cell]
fuelCells = [(x, y) | x <- [1..squareMax], y <- [1..squareMax]]

powerGrid :: Int -> Grid
powerGrid serial = array ((1, 1), (squareMax, squareMax)) $
  zip fuelCells (powerLevel serial <$> fuelCells)

sumForCell :: Grid -> Cell -> State (Map Cell Int) Int
sumForCell grid (x, y) = do
  mp <- get
  if (x, y) `Map.member` mp then
    pure $ mp Map.! (x, y)
  else if x < 1 || y < 1 then
    pure 0
  else do
    let a = grid ! (x, y)
    b <- sumForCell grid (x - 1, y)
    c <- sumForCell grid (x, y - 1)
    d <- sumForCell grid (x - 1, y - 1)
    let s = a + b + c - d
    put (Map.insert (x, y) s mp)
    pure s

summedAreaGrid :: Grid -> Grid
summedAreaGrid grid =
  array ((1, 1), (squareMax, squareMax)) .
  zip fuelCells .
  flip evalState Map.empty .
  sequence $ (sumForCell grid <$> fuelCells)

squarePower :: Int -> Grid -> (Int, Int) -> Int
squarePower l sumGrid (x, y)
  | x == 1 && y == 1 = sumGrid ! (x', y')
  | x == 1 = sumGrid ! (x', y') - sumGrid ! (x', y - 1)
  | y == 1 = sumGrid ! (x', y') - sumGrid ! (x - 1, y')
  | otherwise = sumGrid ! (x', y') - sumGrid ! (x - 1, y')
                - sumGrid ! (x', y - 1) + sumGrid ! (x - 1, y - 1)
  where (x', y') = (x + l - 1, y + l - 1)

maxSquareOfSize :: Int -> Grid -> ((Int, Int), Int)
maxSquareOfSize size sumGrid =
  maximumBy (comparing snd) .
  zip squares .
  fmap (squarePower size sumGrid) $ squares
  where squares =
          [ (x, y)
          | x <- [1..squareMax - size + 1]
          , y <- [1..squareMax - size + 1]
          ]

maxSquare :: Grid -> (Int, Int, Int)
maxSquare sumGrid =
  (\(size, ((x, y), _)) -> (x, y, size)) .
  maximumBy (comparing (snd . snd)) .
  zip [1..squareMax] .
  fmap (`maxSquareOfSize` sumGrid) $ [1..squareMax]

squareMax :: Int
squareMax = 300

run :: IO ()
run = do
  let serial = 1308
      sumGrid = summedAreaGrid . powerGrid $ serial
  uncurry (printf "Part 1: %d,%d\n") . fst $ maxSquareOfSize 3 sumGrid
  (\(x, y, size) -> printf "Part 1: %d,%d,%d\n" x y size) $ maxSquare sumGrid
