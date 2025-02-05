{-# LANGUAGE BangPatterns #-}

module Searches where

import qualified Board as B
import Data.List
import Data.Maybe
import Eval
import qualified Moves as M

negaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
negaMax eval gs depth
  | depth == 0 = Nothing
  | null moves = Nothing
  | otherwise =
    let !bestMove =
          foldl'
            (\acc@(score, _) m ->
               let !score' = negaMax' (M.doMove gs m) (depth - 1) c
                in if score' > score
                     then (score', Just m)
                     else acc)
            (-roadWin, Nothing)
            moves
     in snd bestMove
  where
    !moves = M.generateAllMoves gs
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1
    negaMax' :: B.GameState -> Int -> Int -> Int
    negaMax' gs' depth' color
      | depth' == 0 = color * eval gs'
      | isJust r = (fromJust r) * color
      | otherwise =
        foldl'
          (\alpha m ->
             max alpha (-negaMax' (M.doMove gs' m) (depth' - 1) (-color)))
          (-roadWin)
          moves
      where
        !moves = M.generateAllMoves gs'
        !r = checkForWinScore gs'

alphaBeta :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
alphaBeta eval gs depth
  | depth == 0 = Nothing
  | null moves = Nothing
  | otherwise =
    let !bestMove =
          foldl'
            (\acc@(score, _) m ->
               let newScore =
                     alphabeta' (M.doMove gs m) (depth - 1) c (-roadWin) roadWin
                in if newScore > score
                     then (newScore, Just m)
                     else acc)
            (-roadWin, Nothing)
            moves
     in snd bestMove
  where
    !moves = M.generateAllMoves gs
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1
    alphabeta' :: B.GameState -> Int -> Int -> Int -> Int -> Int
    alphabeta' gs' depth' color alpha beta
      | depth' == 0 = color * eval gs'
      | isJust r = (fromJust r) * color
      | otherwise =
        foldl'
          (\a m ->
             if a >= beta
               then a
               else max
                      a
                      (-alphabeta'
                         (M.doMove gs' m)
                         (depth' - 1)
                         (-color)
                         (-beta)
                         (-a)))
          alpha
          moves
      where
        !r = checkForWinScore gs'
        !moves = M.generateAllMoves gs'
