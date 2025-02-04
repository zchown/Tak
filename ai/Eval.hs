module Eval where

import qualified Board as B
import qualified Moves as M

roadWin = (maxBound :: Int) - 1

flatWin = roadWin - 1

stupidEval :: B.GameState -> Int
stupidEval gs@(B.GameState b c _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ ->
      length (M.controlledPositions b (B.flipColor c)) -
      length (M.controlledPositions b c)

checkForWinScore :: B.GameState -> Maybe Int
checkForWinScore gs
  | r == B.Road B.White = Just $ roadWin
  | r == B.Road B.Black = Just $ -roadWin
  | r == B.FlatWin B.White = Just $ flatWin
  | r == B.FlatWin B.Black = Just $ -flatWin
  | otherwise = Nothing
  where
    r = B.checkGameResult gs
