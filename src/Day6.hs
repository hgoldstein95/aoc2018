{-# LANGUAGE LambdaCase #-}

module Day6 where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List.Split (split, whenElt, dropDelims, dropBlanks)
import Data.List (sortBy, groupBy, maximumBy, minimumBy, filter, group, sort)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.Printf (printf)


type Point = (Int, Int)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

($$) :: (a -> a -> b) -> [a] -> b
f $$ (x : y : _) = f x y

parse :: String -> Point
parse s = (,) $$ (read <$> split splitAtInts s)
  where splitAtInts = dropBlanks . dropDelims $ whenElt (not . isDigit)

compareFrom :: Point -> Point -> Point -> Ordering
compareFrom o l r = compare
  ((fst l - fst o) * (snd r - snd o))
  ((snd l - snd o) * (fst r - fst o))

distanceFrom :: Point -> Point -> Float
distanceFrom (x1, y1) (x2, y2) =
  sqrt (fromIntegral ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)))

dropConcavities :: [Point] -> [Point] -> [Point]
dropConcavities (left:lefter) (right:righter:rightest) =
  case compareFrom left right righter of
    LT -> dropConcavities (right : left : lefter) (righter : rightest)
    EQ -> dropConcavities (left : lefter) (righter : rightest)
    GT -> dropConcavities lefter (left : righter : rightest)
dropConcavities output lastInput = lastInput ++ output

convexHull :: [Point] -> [Point]
convexHull points =
  let anchor = minimum points
      presorted = sortBy (compareFrom anchor) (filter (/= anchor) points)
      collinears = groupBy (((EQ ==) .) . compareFrom anchor) presorted
      outmost = maximumBy (comparing (distanceFrom anchor)) <$> collinears
  in dropConcavities [anchor] outmost

region :: [Point] -> [Point]
region ps = [(x, y) | x <- [xmin..xmax] , y <- [ymin..ymax]]
  where
    (xs, ys) = unzip ps
    (xmin, ymin) = (minimum xs, minimum ys)
    (xmax, ymax) = (maximum xs, maximum ys)

assignPoint :: [Point] -> Point -> Maybe Point
assignPoint ps pt =
  singletonMaybe .
  head .
  groupBy ((==) `on` manhattan pt) .
  sortBy (comparing (manhattan pt)) $ ps
  where singletonMaybe = \case { [x] -> Just x;  _ -> Nothing }

findLargestArea :: [Point] -> Int
findLargestArea ps =
  maximum .
  map length .
  group .
  sort .
  filter (not . (`elem` hull)) .
  catMaybes .
  fmap (assignPoint ps) .
  region $ ps
  where hull = convexHull ps

assignTotalDist :: [Point] -> Point -> Int
assignTotalDist ps pt = sum (manhattan pt <$> ps)

findSizeOfSafeRegion :: [Point] -> Int
findSizeOfSafeRegion ps =
  length . filter (< 10000) . fmap (assignTotalDist ps) . region $ ps

run :: IO ()
run = do
  points <- fmap parse . lines <$> readFile "data/Day6.txt"
  printf "Part 1: %d\n" $ findLargestArea points
  printf "Part 2: %d\n" $ findSizeOfSafeRegion points
