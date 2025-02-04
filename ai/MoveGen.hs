{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module MoveGen where

import qualified Board as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
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
