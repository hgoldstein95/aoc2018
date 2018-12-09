{-# LANGUAGE TemplateHaskell #-}

module Day7.Types where

import Control.Lens (makeLenses)
import Control.Monad.State (State)
import Data.Map (Map)

type Node = Char
type Edge = (Node, Node)
type Graph = Map Node [Node]

data Elf = Elf
  { _currentTask :: Maybe Node
  , _timeLeft :: Int
  } deriving (Show)

data BuildState = BuildState
  { _depGraph :: Graph
  , _time :: Int
  , _elves :: [Elf]
  , _inProgressTasks :: [Node]
  , _finishedTasks :: [Node]
  , _workLog :: [String]
  } deriving (Show)

type Build = State BuildState

makeLenses ''Elf
makeLenses ''BuildState
