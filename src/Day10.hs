{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day10 where

import Control.Lens (use)
import Control.Applicative ((<|>))
import Control.Lens.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Data.IORef
import Data.Maybe
import Data.List
import Snap
import Data.Char (isDigit)
import Data.List.Split (split, dropBlanks, dropDelims, whenElt)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B

import Day10.Types


parse :: String -> (Point, Velocity)
parse s =
  let [x, y, dx, dy] = read <$> split splitAtNums s
  in (Point x y, Velocity dx dy)
  where
    splitAtNums = dropBlanks . dropDelims $ whenElt (not . validChar)
    validChar c = isDigit c || c == '-'

update :: (Point, Velocity) -> (Point, Velocity)
update (Point x y, v@(Velocity dx dy)) = (Point (x + dx) (y + dy), v)

generateFrames :: IO [Frame]
generateFrames = do
  model <- fmap parse . lines <$> readFile "data/Day10.txt"
  let frames =
        fmap (uncurry Frame) . zip [0, 1..] . fmap (fmap fst)
        $ iterate (map update) model
  pure frames

appInit :: SnapletInit  App App
appInit = makeSnaplet "aoc-day-10" "Advent of Code Day 10" Nothing $ do
  addRoutes [ ("next", method POST nextHandler)
            , ("prev", method POST prevHandler)
            , ("reset", method POST resetHandler)
            , ("curr", method GET currHandler)
            ]
  frames <- liftIO $ generateFrames >>= newIORef
  prev <- liftIO $ newIORef []
  return App { _frames = frames, _prevFrames = prev }

type Movement a = Int -> [a] -> [a] -> ([a], [a])

moveNFwd :: Movement a
moveNFwd n xs ys = (reverse xs' ++ xs, ys')
  where (xs', ys') = splitAt n ys

moveNBck :: Movement a
moveNBck n xs ys = (xs', reverse ys' ++ ys)
  where (ys', xs') = splitAt n xs

moveHandler :: Movement Frame -> Handler App App ()
moveHandler mvmt = do
  count <- getPostParam "count" >>= \case
    Nothing -> error "ack"
    Just n -> pure . read $ B.unpack n
  frameRef <- use frames
  prevRef <- use prevFrames
  liftIO $ do
    fs <- readIORef frameRef
    ps <- readIORef prevRef
    let (ps', fs') = mvmt count ps fs
    writeIORef frameRef fs'
    writeIORef prevRef ps'
  currHandler

nextHandler :: Handler App App ()
nextHandler = moveHandler moveNFwd

prevHandler :: Handler App App ()
prevHandler = moveHandler moveNBck

resetHandler :: Handler App App ()
resetHandler = do
  frameRef <- use frames
  prevRef <- use prevFrames
  liftIO $ do
    generateFrames >>= writeIORef frameRef
    writeIORef prevRef []
  currHandler

currHandler :: Handler App App ()
currHandler = do
  frameRef <- use frames
  f <- liftIO $ head <$> readIORef frameRef
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  writeLBS . encode $ f

run :: IO ()
run = serveSnaplet defaultConfig appInit
