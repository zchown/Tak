module Searches where

import qualified Board as B
import qualified Data.Vector as V
import Eval
import qualified Moves as M

negaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Int
negaMax eval gs depth
  | depth == 0 = eval gs
  | otherwise = maximum $ V.map foo (M.generateAllMoves gs)
  where
    foo m = -negaMax eval (M.doMove gs m) (depth - 1)
