{-# LANGUAGE BangPatterns #-}

module Searches where

import qualified Board as B
import qualified Data.Vector as V
import Eval
import qualified Moves as M

negaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
negaMax eval gs depth = do
  let !moves = M.generateAllMoves gs
  if null moves
    then Nothing
    else let !scoredMoves =
               V.map (\m -> (m, negaMax' (M.doMove gs m) (depth - 1) c)) moves
             !bestMove =
               V.maximumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves
          in Just $ fst bestMove
  where
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1
    negaMax' :: B.GameState -> Int -> Int -> Int
    negaMax' gs' depth' color
      | depth' == 0 = color * eval gs'
      | otherwise =
        V.foldl'
          (\alpha m ->
             max alpha (-negaMax' (M.doMove gs' m) (depth' - 1) (-color)))
          (-roadWin)
          moves
      where
        moves = V.reverse $ M.generateAllMoves gs'

alphaBetaNegaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
alphaBetaNegaMax eval gs depth = do
  let !moves = M.generateAllMoves gs
  if null moves
    then Nothing
    else let !scoredMoves =
               V.map
                 (\m ->
                    ( m
                    , alphaBetaNegaMax'
                        eval
                        (M.doMove gs m)
                        (depth - 1)
                        c
                        (-roadWin)
                        roadWin))
                 moves
             !bestMove =
               V.maximumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves
          in Just $ fst bestMove
  where
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1

alphaBetaNegaMax' ::
     (B.GameState -> Int) -> B.GameState -> Int -> Int -> Int -> Int -> Int
alphaBetaNegaMax' eval gs' depth' color alpha beta
  | depth' == 0 = color * eval gs'
  | otherwise =
    let !moves = M.generateAllMoves gs'
     in if null moves
          then 0
          else V.foldl'
                 (\alpha' m ->
                    let !score =
                          -alphaBetaNegaMax'
                            eval
                            (M.doMove gs' m)
                            (depth' - 1)
                            (-color)
                            (-beta)
                            (-alpha')
                     in if score >= beta
                          then beta
                          else max alpha' score)
                 alpha
                 moves
