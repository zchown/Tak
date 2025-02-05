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
import qualified System.Random.Shuffle as RS
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

stupidEval1 :: B.GameState -> IO Text
stupidEval1 gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      moves' <- RS.shuffleM (V.toList moves)
      let scoredMoves =
            V.map (\m -> (m, E.stupidEval (M.doMove gs m))) (V.fromList moves')
      let bestMove = V.maximumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves
      return $ PTN.moveToText (fst bestMove)

betterEval1 :: B.GameState -> IO Text
betterEval1 gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      moves' <- RS.shuffleM (V.toList moves)
      let scoredMoves =
            V.map (\m -> (m, E.betterEval (M.doMove gs m))) (V.fromList moves')
      let bestMove = V.minimumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves
      return $ PTN.moveToText (fst bestMove)

-- use negaMax to find the best move
betterEval3 :: B.GameState -> IO Text
betterEval3 gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      let moves' =
            V.map (\m -> (m, -S.negaMax E.betterEval (M.doMove gs m) 1)) moves
      let bestMove = V.maximumBy (\(_, s1) (_, s2) -> compare s1 s2) moves'
      return $ PTN.moveToText (fst bestMove)
