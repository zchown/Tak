{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module MoveGen where

import qualified Board as B
import Data.Text (Text)
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
      let move = moves !! randomIndex
      return $ PTN.moveToText move

percentageRandomMove ::
     (B.GameState -> IO Text) -> Int -> B.GameState -> IO Text
percentageRandomMove policy percent gs = do
  randomNum <- randomRIO (1, 100)
  if randomNum >= percent
    then do
      putStrLn $ "Random move generated: " ++ show randomNum
      generateRandomMove gs
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
betterEval = generatorPattern S.negaMax E.betterEval 3

bestEval :: B.GameState -> IO Text
bestEval = generatorPattern S.negaMax E.bestEval 3

alphaBetaBest :: B.GameState -> IO Text
alphaBetaBest = generatorPattern S.alphaBeta E.bestEval 3

alphaBetaBest' :: B.GameState -> IO Text
alphaBetaBest' = generatorPattern S.alphaBeta E.bestEval' 3

alphaBetaBestFast :: B.GameState -> IO Text
alphaBetaBestFast = generatorPattern S.alphaBeta E.bestEval' 1

alphaBetaBest50' :: B.GameState -> IO Text
alphaBetaBest50' = percentageRandomMove alphaBetaBest' 50

alphaBetaStupid :: B.GameState -> IO Text
alphaBetaStupid = generatorPattern S.alphaBeta E.stupidEval 2

alphaBetaStupid90 :: B.GameState -> IO Text
alphaBetaStupid90 = percentageRandomMove alphaBetaBest 90

alphaBetaBetter :: B.GameState -> IO Text
alphaBetaBetter = generatorPattern S.alphaBeta E.betterEval 3

alphaBetaBetter90 :: B.GameState -> IO Text
alphaBetaBetter90 = percentageRandomMove alphaBetaBetter 90

bestEval90 :: B.GameState -> IO Text
bestEval90 = percentageRandomMove bestEval 90

betterEval90 :: B.GameState -> IO Text
betterEval90 = percentageRandomMove betterEval 90

alphaBetaBest90 :: B.GameState -> IO Text
alphaBetaBest90 = percentageRandomMove alphaBetaBest 90
