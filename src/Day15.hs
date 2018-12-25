module Day15 where

import Prelude hiding (round)

import Control.Lens ((^.), (%=), (.=), use, at, set, over, view)
import Control.Monad (unless, when)
import Control.Monad.State.Strict (execState, evalState, runState)
import Data.List (sort, sortBy)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust, isNothing)
import Data.Ord (comparing)
import Text.Printf (printf)

import Day15.Types
import Day15.Parsing
import Day15.Path


moveUnit :: Unit -> [Unit] -> Combat Unit
moveUnit u targets = do
  newLoc <- computeNextLoc u targets
  let u' = set location newLoc u
  cavern.at (u^.location) .= Just Empty
  cavern.at newLoc .= Just (Entity u')
  pure u'

attack :: Unit -> Unit -> Combat ()
attack u v = do
  let v' = over health (subtract (u^.power)) v
  if v'^.health <= 0
    then do
      cavern.at (v^.location) .= Just Empty
      when (v^.alignment == Elf) $ deadElves %= (+ 1)
    else cavern.at (v^.location) .= Just (Entity v')

neighbors :: Loc -> [Loc]
neighbors (At x y) = [At (x + 1) y, At x (y + 1), At (x - 1) y, At x (y - 1)]

moveAndAttack :: Unit -> [Unit] -> Combat ()
moveAndAttack u targets = do
  u' <- if null $ attackableFrom u then moveUnit u targets else pure u
  case attackableFrom u' of
    (v : _) -> attack u' v
    [] -> pure ()
  where
    inAttackRange u v = v^.location `elem` neighbors (u^.location)
    attackableFrom x =
      sortBy (comparing _health) . filter (inAttackRange x) $ targets

turn :: Loc -> Combat Bool
turn l = do
  c <- use cavern
  case getUnit $ c ! l of
    Nothing -> pure False
    Just u -> do
      let targets = sortBy (comparing _location) .
                    filter (\x -> x^.alignment /= u^.alignment) . units $ c
      if null targets
        then pure True
        else moveAndAttack u targets >> pure False

round :: Combat Bool
round = do
  c <- use cavern
  let us = sort . fmap _location . units $ c
  or <$> mapM turn us

battle :: Combat ()
battle = do
  done <- round
  fuel %= subtract 1
  f <- use fuel
  unless (done || f == 0) $ do
    roundNo %= (+ 1)
    battle

computeOutcome :: CombatState -> Int
computeOutcome res =
  (res^.roundNo) * (sum . fmap tileHealth . Map.elems $ res^.cavern)
  where
    tileHealth (Entity u) = u^.health
    tileHealth _ = 0

outcome :: Combat () -> CombatState -> Int
outcome combat init =
  computeOutcome $ execState combat init

tryAttackPower :: Combat () -> CombatState -> Int -> Maybe Int
tryAttackPower combat init p =
  let init' = over cavern (fmap setElfPower) init
      (res, final) = runState combat init'
  in if final^.deadElves > 0 then Nothing else Just $ computeOutcome final
  where
    setElfPower Empty = Empty
    setElfPower Rock = Rock
    setElfPower (Entity u)
      | u^.alignment == Elf = Entity $ set power p u
      | otherwise = Entity u

findMinElfOutcome :: Combat () -> CombatState -> Int
findMinElfOutcome combat init =
  fromJust . head . dropWhile isNothing .
  fmap (tryAttackPower combat init) $ [4, 5..]

evalCombat :: Int -> CombatState -> Combat a -> a
evalCombat n cs = flip evalState (set fuel n cs)

runTest :: FilePath -> Int -> IO Bool
runTest path expected = do
  test <- initialState <$> readFile path
  let res = outcome battle test
  if res == expected then pure True else do
    printf "Failure at %s. Expected: %d, Actual: %d\n" path expected res
    pure False

runTests :: IO Bool
runTests = and <$> sequence
  [ runTest "data/Day15/test0.txt" 27730
  , runTest "data/Day15/test1.txt" 36334
  , runTest "data/Day15/test2.txt" 39514
  , runTest "data/Day15/test3.txt" 27755
  , runTest "data/Day15/test4.txt" 28944
  , runTest "data/Day15/test5.txt" 18740
  ]

run :: IO ()
run = do
  pass <- runTests
  if not pass then putStrLn "Test(s) failed." else do
    init <- initialState <$> readFile "data/Day15.txt"
    printf "Part 1: %d\n" $ outcome battle init
    printf "Part 2: %d\n" $ findMinElfOutcome battle init
