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

betterEval3 :: B.GameState -> IO Text
betterEval3 = generatorPattern S.alphaBetaNegaMax E.betterEval 3

stupidEval3 :: B.GameState -> IO Text
stupidEval3 = generatorPattern S.alphaBetaNegaMax E.stupidEval 3
