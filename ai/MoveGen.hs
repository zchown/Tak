{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module MoveGen where

import qualified Board as B
import Data.Either (fromRight)
import Data.Text (Text)
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

-- use negaMax to find the best move
betterEval3 :: B.GameState -> IO Text
betterEval3 gs = S.negaMax E.betterEval gs 2

stupidEval3 :: B.GameState -> IO Text
stupidEval3 gs = S.alphaBetaNegaMax E.stupidEval gs 3
