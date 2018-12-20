{-# LANGUAGE LambdaCase #-}

module Day12 where

import Control.Monad ((=<<))
import Data.List (zip5, unzip5, zip3)
import Text.Printf (printf)

data Pot
  = Empty
  | Plant
  deriving (Eq)

instance Show Pot where
  show Empty = "."
  show Plant = "#"

data Pattern = Pattern [Pot] Pot

instance Show Pattern where
  show (Pattern l r) = (show =<< l) ++ " => " ++ show r

newtype Cavern = Cavern { unCavern :: [(Int, Pot)] }

instance Show Cavern where
  show (Cavern xs) = show =<< (snd <$> xs)

mkPot :: Char -> Pot
mkPot '#' = Plant
mkPot '.' = Empty

parseInitial :: String -> Cavern
parseInitial =
  Cavern .
  zip [-magicNumberL, -(magicNumberL - 1)..] .
  (replicate magicNumberL Empty ++) .
  (++ replicate magicNumberR Empty) .
  fmap mkPot . (!! 2) . words

parsePattern :: String -> Pattern
parsePattern s = Pattern (mkPot <$> l) (mkPot . head $ r)
  where [l, _, r] = words s

data Focus
  = Unmatched (Pot, Pot, (Int, Pot), Pot, Pot)
  | Done (Int, Pot)

unFocus :: [Focus] -> Cavern
unFocus = Cavern . fmap
  (\case (Unmatched (_, _, (i, _), _, _)) -> (i, Empty); (Done p) -> p)

apply
  :: Pattern
  -> Focus
  -> Focus
apply _ d@(Done _) = d
apply (Pattern l r) item@(Unmatched (a, b, (i, c), d, e)) =
  if l == [a, b, c, d, e] then Done (i, r) else item

evolve :: [Pattern] -> Cavern -> Cavern
evolve ps (Cavern cavern) = unFocus . foldr (\p fs -> apply p <$> fs) fives $ ps
  where pots = snd <$> cavern
        fives = Unmatched <$>
          zip5 (Empty : Empty : pots) (Empty : pots) cavern
          (drop 1 (pots ++ [Empty])) (drop 2 pots ++ [Empty, Empty])

score :: Cavern -> Int
score = sum . fmap fst . filter ((== Plant) . snd) . unCavern

magicNumberL :: Int
magicNumberL = 10

magicNumberR :: Int
magicNumberR = 60

run :: IO ()
run = do
  (x : _ : xs) <- lines <$> readFile "data/Day12.txt"
  let init = parseInitial x
      pats = parsePattern <$> xs
      generations = iterate (evolve pats) init
  mapM_ printTup . take 50
    $ zip3 [0 :: Int, 1..] (score <$> generations) generations
  where printTup (n, s, t) = printf "%d\t%d\t%s\n" n s (show t)
