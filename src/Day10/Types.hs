{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10.Types where

import Control.Lens (makeLenses)
import Data.IORef (IORef)
import Data.Aeson

data Point = Point Int Int
  deriving (Show)

data Velocity = Velocity Int Int
  deriving (Show)

data Frame = Frame Int [Point]

data App = App
  { _frames :: IORef [Frame]
  , _prevFrames :: IORef [Frame]
  }

instance ToJSON Point where
  toJSON (Point x y) = object [ "x" .= x, "y" .= y ]

instance ToJSON Frame where
  toJSON (Frame n ps) = object [ "step" .= n, "points" .= ps ]

makeLenses ''App
