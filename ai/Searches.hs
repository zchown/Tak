{-# LANGUAGE BangPatterns #-}

module Searches where

import qualified Board as B
import qualified Data.Vector as V
import Eval
import qualified Moves as M

negaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
negaMax eval gs depth
  | depth == 0 = Nothing
  | null moves = Nothing
  | otherwise =
    let !bestMove =
          V.foldl'
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
      | r /= 0 = r
      | otherwise =
        V.foldl'
          (\alpha m ->
             max alpha (-negaMax' (M.doMove gs' m) (depth' - 1) (-color)))
          (-roadWin)
          moves
      where
        !moves = M.generateAllMoves gs'
        !r =
          case B.checkGameResult gs' of
            B.FlatWin B.White -> color * flatWin
            B.FlatWin B.Black -> -color * (-flatWin)
            B.Road B.White -> color * roadWin
            B.Road B.Black -> -color * (-roadWin)
            _ -> 0
