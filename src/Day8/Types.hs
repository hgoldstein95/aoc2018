{-# LANGUAGE TemplateHaskell #-}

module Day8.Types where

import Control.Lens (makeLenses)
import Control.Monad.State (State, evalState)


data Tree = Node
  { _children :: [Tree]
  , _metadata :: [Int]
  } deriving (Show)

makeLenses ''Tree
