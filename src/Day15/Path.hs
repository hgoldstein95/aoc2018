{-# LANGUAGE FlexibleContexts #-}

module Day15.Path where

import Control.Lens ((^.), use, view)
import Control.Monad (filterM)
import Control.Monad.State.Strict (evalState, get, put)
import Data.List (minimumBy, delete, nub, sort, sortBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set

import Day15.Types


computeNextLoc :: Unit -> [Unit] -> Combat Loc
computeNextLoc u targets = do
  moves <- validMoves
  let ends = concatMap (moves . view location) targets
  pure $ nextStepToTarget moves (u^.location) ends

validMoves :: Combat (Loc -> [Loc])
validMoves = do
  c <- use cavern
  (xmax, ymax) <- use bounds
  pure $ \(At u v) ->
    filter (checkStep c xmax ymax)
    [At u (v - 1), At (u - 1) v, At (u + 1) v, At u (v + 1)]
  where
    checkStep c xmax ymax (At x y) = and
      [ x >= 0, y >= 0
      , x <= xmax, y <= ymax
      , c ! At x y == Empty
      ]

nextStepToTarget :: (Loc -> [Loc]) -> Loc -> [Loc] -> Loc
nextStepToTarget moves s targets =
  case evalState (go [[s]]) (Set.singleton s) of
    [] -> s
    paths -> head . minimumBy (comparing length) $ paths
  where
    go paths = do
      paths' <- concat <$> mapM stepPath paths
      case filter endsInTarget paths' of
        [] -> if null paths' then pure [] else go paths'
        xs -> pure . sortBy (comparing head) . fmap (tail . reverse) $ xs
    stepPath path = do
      v <- get
      let paths = [loc : path | loc <- moves (head path), loc `Set.notMember` v]
      put (v `Set.union` Set.fromList (head <$> paths))
      pure paths
    endsInTarget = (`elem` targets) . head
