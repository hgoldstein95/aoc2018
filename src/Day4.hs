module Day4 where

import Day4.DateTime

import Control.Arrow (second, (&&&))
import Control.Lens ((^.))
import Data.Map (Map, (!))
import Data.Bool (bool)
import Data.Functor (($>))
import Data.List (sortBy, groupBy, maximumBy)
import Data.List.Split (split, keepDelimsL, dropInitBlank, whenElt)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.Char
import Text.Printf (printf)

import qualified Data.Map as Map

type Guard = Int
type Minute = Int

data Message
  = StartShift Guard
  | FallAsleep
  | WakeUp
  deriving (Show, Eq)

isStart :: Message -> Bool
isStart (StartShift _) = True
isStart _ = False

type Observation = (DateTime, Message)

type Night = [Bool]

parseObservation :: String -> Observation
parseObservation = either (error "Invalid observation") id . parse obsP ""
  where
    obsP = (,) <$> dateTimeP <*> (space *> messageP)
    messageP = startShiftP <|> fallAsleepP <|> wakeUpP
    startShiftP = StartShift <$>
      (string "Guard #" *> uintP <* string " begins shift")
    fallAsleepP = string "falls asleep" $> FallAsleep
    wakeUpP = string "wakes up" $> WakeUp

processObservations :: [Observation] -> (Guard, Night)
processObservations ((_, StartShift guard) : os) = (guard, go [0..59] os False)
  where
    go (t : ts) (o@(ot, _) : os) isAsleep
      | t /= ot^.time.minutes = isAsleep : go ts (o : os) isAsleep
    go (_ : ts) ((_, FallAsleep) : os) _ = True : go ts os True
    go (_ : ts) ((_, WakeUp) : os) _ = False : go ts os False
    go (_ : ts) [] isAsleep = isAsleep : go ts [] isAsleep
    go _ _ _ = []
processObservations os = error $ "Could not process observations " ++ show os

sleepiestGuard :: Map Guard [Night] -> Guard
sleepiestGuard =
  fst . maximumBy (comparing snd) . fmap (second timeAsleep) . Map.assocs
  where timeAsleep = sum . map (length . filter id)

sleepiestMinute :: [Night] -> (Minute, Int)
sleepiestMinute arrs =
  maximumBy (comparing snd) $ zip [0..59] (map timeAsleep [0..59])
  where timeAsleep i = length . filter id $ map (!! i) arrs

worstGuardMinute :: Map Guard [Night] -> (Guard, Minute)
worstGuardMinute mp =
  id &&& (fst . sleepiestMinute . (mp !)) $ sleepiestGuard mp

frequentSleepyMinute :: Map Guard [Night] -> (Guard, Minute)
frequentSleepyMinute =
  (fst &&& (fst . snd)) .
  maximumBy (comparing (snd . snd)) .
  Map.assocs .
  fmap sleepiestMinute

run :: IO ()
run = do
  obs <- split (dropInitBlank . keepDelimsL $ whenElt (isStart . snd)) .
         sortBy (comparing fst) .
         fmap parseObservation .
         lines <$> readFile "data/Day4.txt"
  let guardShifts = Map.fromListWith (++)
        [(g, [n]) | (g, n) <- processObservations <$> obs]
  printf "Part 1: %d\n" . uncurry (*) $ worstGuardMinute guardShifts
  printf "Part 2: %d\n" . uncurry (*) $ frequentSleepyMinute guardShifts
