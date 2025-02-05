module Eval where

import qualified Board as B
import Data.Matrix
import qualified Moves as M

roadWin = 100000

flatWin = roadWin - 1

stupidEval :: B.GameState -> Int
stupidEval gs@(B.GameState b c _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ ->
      length (M.controlledPositions b B.White) -
      length (M.controlledPositions b B.Black)

betterEval :: B.GameState -> Int
betterEval gs@(B.GameState b _ _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ -> (2 * fcd) + pc
  where
    fcd = getFlatCount b B.White - getFlatCount b B.Black
    pc = getPrisonerCount b B.White - getPrisonerCount b B.Black

--------------------------
-- | Helper Functions | --
---------------------------
checkForWinScore :: B.GameState -> Maybe Int
checkForWinScore gs
  | r == B.Road B.White = Just $ roadWin
  | r == B.Road B.Black = Just $ -roadWin
  | r == B.FlatWin B.White = Just $ flatWin
  | r == B.FlatWin B.Black = Just $ -flatWin
  | otherwise = Nothing
  where
    r = B.checkGameResult gs

getFlatCount :: B.Board -> B.Color -> Int
getFlatCount board color =
  length $ filter (isFlat) (M.controlledPositions board color)
  where
    isFlat :: B.Position -> Bool
    isFlat (B.Position (x, y)) = B.ps (head (getElem x y board)) == B.Flat

getPrisonerCount :: B.Board -> B.Color -> Int
getPrisonerCount board color = sum $ map foo cps
  where
    cps = M.controlledPositions board color
    foo (B.Position (x, y)) =
      length $ (filter ((/= color) . B.pc) (getElem x y board))
