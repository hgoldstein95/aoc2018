module Day1 where

import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Data.Set as Set


type Delta = Integer -> Integer

parseDelta :: String -> Maybe Delta
parseDelta (sign : num) = do
  n <- readMaybe num
  case sign of
    '+' -> pure $ \x -> x + n
    '-' -> pure $ \x -> x - n
    _ -> Nothing
parseDelta _ = Nothing

firstDup :: (Ord a, Eq a) => [a] -> Maybe a
firstDup = go Set.empty
  where
    go _ [] = Nothing
    go s (x : xs) | x `Set.member` s = Just x
    go s (x : xs) = go (x `Set.insert` s) xs

run :: IO ()
run = do
  deltas <- mapMaybe parseDelta . lines <$> readFile "data/Day1.txt"
  printf "Part 1: %d\n" $ foldl (&) 0 deltas
  let freqs = scanl (&) 0 (cycle deltas)
  printf "Part 2: %d\n" . fromJust . firstDup $ freqs
