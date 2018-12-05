{-# LANGUAGE TemplateHaskell #-}

module Day4.DateTime where

import Control.Lens (makeLenses)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator


data Date = Date
  { _year :: Int
  , _month :: Int
  , _day :: Int
  } deriving (Show, Eq)

data Time = Time
  { _hours :: Int
  , _minutes :: Int
  , _seconds :: Int
  } deriving (Show, Eq)

data DateTime = DateTime
  { _date :: Date
  , _time :: Time
  } deriving (Show, Eq)

instance Ord Date where
  (Date y1 m1 d1) `compare` (Date y2 m2 d2) =
    (y1, m1, d1) `compare` (y2, m2, d2)

instance Ord Time where
  (Time h1 m1 s1) `compare` (Time h2 m2 s2) =
    (h1, m1, s1) `compare` (h2, m2, s2)

instance Ord DateTime where
  (DateTime d1 t1) `compare` (DateTime d2 t2) = (d1, t1) `compare` (d2, t2)


uintP :: Parsec String u Int
uintP = read <$> many1 digit

dateP :: Parsec String u Date
dateP = Date <$> uintP <*> (char '-' *> uintP) <*> (char '-' *> uintP)

timeP :: Parsec String u Time
timeP = Time <$> uintP <*> (char ':' *> uintP) <*> secP
  where secP = (char ':' *> uintP) <|> pure 0

dateTimeP :: Parsec String u DateTime
dateTimeP = between (char '[') (char ']') $
            DateTime <$> dateP <*> (space *> timeP)

makeLenses ''Date
makeLenses ''Time
makeLenses ''DateTime
