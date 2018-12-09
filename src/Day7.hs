{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Day7 where

import Prelude hiding (log)
import Control.Lens

import Control.Arrow (first, (&&&))
import Control.Monad (when, unless)
import Control.Monad.State (execState, get)
import Data.Char (ord)
import Data.Either (either)
import Data.List ((\\), nub, concat, sort, filter, sortBy, delete)
import Data.Map ((!))
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import Text.Parsec (parse)
import Text.Parsec.Char (string, anyChar)
import Text.Printf (printf)

import qualified Data.Map as Map

import Day7.Types


parseEdge :: String -> Edge
parseEdge = either (error "Bad parse") id . parse edgeP ""
  where edgeP = (,) <$> (string "Step " *> anyChar)
          <*> (string " must be finished before step "
               *> anyChar <* string " can begin.")

mkGraph :: [Edge] -> Graph
mkGraph es = foldr addEdge initMap es
  where
    nodes = sort . nub . uncurry (++) . unzip $ es
    initMap = Map.fromList ((, []) <$> nodes)
    addEdge (n, m) = Map.update (Just . (m :)) n

sortSteps :: [Edge] -> [Node]
sortSteps es = go []
  where
    graph = mkGraph es
    nodes = sort $ Map.keys graph
    newReady done = sort $ filter (all (`elem` done) . (graph !)) nodes \\ done
    go done =
      case newReady done of
        [] -> []
        (r : _) -> r : go (r : done)

timeForStep :: Node -> Int
timeForStep = (+ baseTime) . subtract 64 . ord

log :: String -> Build ()
log s = do
  t <- use time
  workLog %= ((show t ++ ":\t" ++ s) :)

nextTask :: Build (Maybe Node)
nextTask = do
  graph <- use depGraph
  finished <- use finishedTasks
  inProgress <- use inProgressTasks
  let valid = fmap fst . filter (all (`elem` finished) . snd) $ Map.assocs graph
  pure $ case sort . (\\ inProgress) . (\\ finished) $ valid of
    [] -> Nothing
    (x : _) -> Just x

elfWork :: Int -> Build ()
elfWork elfIndex = do
  -- Run a step of the current task
  use (self.currentTask) >>= \case
    Just task -> do
      self.timeLeft %= subtract 1
      t <- use (self.timeLeft)
      when (t <= 0) $ do
        log $ printf "Elf %d finished %c" elfIndex task
        finishedTasks %= (task :)
        inProgressTasks %= delete task
        self.currentTask .= Nothing
        self.timeLeft .= 0

        done <- use finishedTasks
        log $ "Done: " ++ reverse done
    Nothing -> pure ()

  -- Get a new task
  task <- use (self.currentTask)
  when (isNothing task) $ nextTask >>= \case
    Nothing -> pure ()
    Just task -> do
      log $ printf "Elf %d chose next task %c" elfIndex task
      inProgressTasks %= (task :)
      self.currentTask .= Just task
      self.timeLeft .= timeForStep task

  where
    self :: Lens' BuildState Elf
    self = lens getSelf setSelf
    getSelf = (!! elfIndex) . view elves
    setSelf st v = over elves (imap (\i e -> if i == elfIndex then v else e)) st

buildSleigh :: Build ()
buildSleigh = do
  allTasks <- Map.keys <$> use depGraph
  es <- use elves
  done <- use finishedTasks
  unless (all (`elem` done) allTasks) $ do
    mapM_ elfWork [0..length es - 1]
    time %= (+ 1)
    buildSleigh

timeBuild :: Graph -> Build a -> (Int, [String])
timeBuild g b =
  (view time &&& reverse . view workLog) $ execState b initBuildState
  where
    initElf = Elf
      { _currentTask = Nothing
      , _timeLeft = 0
      }
    initBuildState = BuildState
      { _depGraph = g
      , _time = -1
      , _elves = replicate numberOfElves initElf
      , _inProgressTasks = []
      , _finishedTasks = []
      , _workLog = []
      }

numberOfElves :: Int
numberOfElves = 5

baseTime :: Int
baseTime = 60

shouldLog :: Bool
shouldLog = False

run :: IO ()
run = do
  edges <- fmap (swap . parseEdge) . lines <$> readFile "data/Day7.txt"
  printf "Part 1: %s\n" . sortSteps $ edges
  let (endTime, logLines) = timeBuild (mkGraph edges) buildSleigh
  when shouldLog $ mapM_ putStrLn logLines
  printf "Part 2: %d\n" endTime
