module Day3 where

import Data.Char (isDigit)
import Data.List (group, sort, head, find, (\\), tails, nub)
import Data.List.Split (split, dropBlanks, whenElt, dropDelims)
import Data.Maybe (fromJust)
import Text.Parsec (parse, many)
import Text.Parsec.Char (string, char, digit)
import Text.Printf (printf)


type Cell = (Int, Int)

data Claim = Claim
  { claimId :: Int
  , claimOrigin :: Cell
  , claimWidth :: Int
  , claimHeight :: Int
  } deriving (Show, Eq)

parseClaim :: String -> Claim
parseClaim s =
  let [gid, x, y, w, h] = map read $ splitter s
  in Claim gid (x, y) w h
  where splitter = split (dropBlanks . dropDelims $ whenElt (not . isDigit))

countOverlaps :: [Claim] -> Int
countOverlaps claims =
  length . filter ((> 1) . length) . group . sort $ cells =<< claims
  where cells (Claim _ (x, y) w h) =
          [ (x', y')
          | x' <- [x..x + w - 1]
          , y' <- [y..y + h - 1]
          ]

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

bestClaim :: [Claim] -> Claim
bestClaim claims = fromJust $ find (not . (`elem` claimsWithOverlap)) claims
  where claimsWithOverlap =
          uncurry (++) . unzip . filter (uncurry overlap) $ pairs claims
        overlap (Claim _ (ax, ay) aw ah) (Claim _ (bx, by) bw bh) = and
          [ ax < bx + bw
          , ax + aw > bx
          , ay < by + bh
          , ay + ah > by
          ]

run :: IO ()
run = do
  claims <- fmap parseClaim . lines <$> readFile "data/Day3.txt"
  printf "Part 1: %d\n" $ countOverlaps claims
  printf "Part 2: %d\n" . claimId $ bestClaim claims
