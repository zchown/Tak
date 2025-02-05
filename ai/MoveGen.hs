{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module MoveGen where

import qualified Board as B
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Eval as E
import qualified Moves as M
import qualified PTN
import qualified Searches as S
import System.Random (randomRIO)

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

percentageRandomMove ::
     (B.GameState -> IO Text) -> Int -> B.GameState -> IO Text
percentageRandomMove policy percent gs = do
  randomNum <- randomRIO (1, 100)
  if randomNum >= percent
    then generateRandomMove gs
    else policy gs

generatorPattern ::
     ((B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move)
  -> (B.GameState -> Int)
  -> Int
  -> B.GameState
  -> IO Text
generatorPattern search eval gs depth = do
  case search eval depth gs of
    Just move -> return $ PTN.moveToText move
    Nothing -> return "No valid moves"

betterEval :: B.GameState -> IO Text
betterEval = generatorPattern S.negaMax E.betterEval 2

stupidEval :: B.GameState -> IO Text
stupidEval = generatorPattern S.alphaBetaNegaMax E.stupidEval 3

bestEval :: B.GameState -> IO Text
bestEval = generatorPattern S.negaMax E.bestEval 2

bestEval80 :: B.GameState -> IO Text
bestEval80 = percentageRandomMove bestEval 80
