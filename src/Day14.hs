{-# LANGUAGE TemplateHaskell #-}

module Day14 where

import Control.Lens (makeLenses, (^.))
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Text.Printf (printf)

type Elves = [Int]

data ScoreBoard = ScoreBoard
  { _len :: Int
  , _scores :: Map Int Int
  } deriving (Show)
makeLenses ''ScoreBoard

pretty :: (Elves, ScoreBoard) -> String
pretty (es, board) = show es ++ " " ++
  unwords (fmap (show . ((board^.scores) !)) [0..board^.len - 1])

addScore :: ScoreBoard -> Int -> ScoreBoard
addScore (ScoreBoard l m) n = ScoreBoard (l + 1) (Map.insert l n m)

digits :: Int -> [Int]
digits = fmap (read . (: [])) . show

step :: (Elves, ScoreBoard) -> (Elves, ScoreBoard)
step (es, board) = (es', board')
  where
    board' = foldl addScore board . digits . sum . fmap ((board^.scores) !) $ es
    updateElf e = (e + ((board'^.scores) ! e) + 1) `mod` board'^.len
    es' = fmap updateElf es

countBefore :: [Int] -> ScoreBoard -> Maybe Int
countBefore xs board
  | length xs + 2 >= board^.len = Nothing
  | otherwise =
    if xs == fmap ((board^.scores) !) range1 then
      Just $ lastIndex - length xs + 1
    else if xs == fmap ((board^.scores) !) range2 then
      Just $ lastIndex - length xs
    else
      Nothing
  where
    lastIndex = (board^.len) - 1
    range1 = [lastIndex - length xs + 1..lastIndex]
    range2 = [lastIndex - length xs..lastIndex - 1]

run :: IO ()
run = do
  let input = [0, 4, 7, 8, 0, 1]
      inputNo = read . (show =<<) $ input
      targetLen = inputNo + 10
      boards = fmap snd . iterate step $
               ([0, 1], foldl addScore (ScoreBoard 0 Map.empty) [3, 7])
      part1Board = head . dropWhile ((< targetLen) . (^.len)) $ boards
      part2Board = head . dropWhile (isNothing . countBefore input) $ boards
  printf "Part 1: %s\n" .
    (show =<<) $ fmap ((part1Board^.scores) !) [inputNo..targetLen - 1]
  printf "Part 2: %d\n" .
    fromJust $ countBefore input part2Board
