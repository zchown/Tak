{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module MoveGen where

import qualified Board as B
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Eval as E
import qualified Moves as M
import qualified PTN
import qualified Searches as S
import System.Random (randomRIO)
import qualified TPS

generateRandomMove :: B.GameState -> IO Text
generateRandomMove gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      randomIndex <- randomRIO (0, length moves - 1)
      putStrLn $ "Moves generated: " ++ show (length moves)
      let move = moves V.! randomIndex
      return $ PTN.moveToText move

doMove :: B.GameState -> B.Move -> B.GameState
doMove gs@(B.GameState b c mn p1 p2 r _) m = do
  case M.makeMove b m of
    Left _ -> gs
    Right newBoard ->
      let newTurn =
            if c == B.White
              then B.Black
              else B.White
          newMoveNum = mn + 1
          (p1', p2') = B.getNewReserves p1 p2 m
          r' = B.checkGameResult gs
       in B.GameState newBoard newTurn newMoveNum p1' p2' r' []

stupidEval1 :: B.GameState -> IO Text
stupidEval1 gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      let scoredMoves = V.map (\m -> (m, E.stupidEval (doMove gs m))) moves
      let bestMove = V.maximumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves
      return $ PTN.moveToText (fst bestMove)
