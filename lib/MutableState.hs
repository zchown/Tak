{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module MutableState where

import qualified Board as B
import Control.Monad (forM, forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Matrix as M
import Data.Maybe (fromJust, isJust)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
import GHC.Generics (Generic)
import qualified Moves

type MBoard s = IOVector B.Square

data MGameState s = MGameState
  { mBoard :: MBoard s
  , mTurn :: IORef B.Color
  , mMoveNumber :: IORef Int
  , mPlayer1 :: IORef B.Reserves
  , mPlayer2 :: IORef B.Reserves
  , mResult :: IORef B.Result
  , mGameHistory :: IORef B.History
  }

createMutableBoard :: B.Board -> IO (MBoard s)
createMutableBoard b = do
  mv <- V.new (rows * cols)
  forM_ [0 .. (rows * cols - 1)] $ \i -> do
    V.write mv i (squares !! i)
  return mv
  where
    rows = M.nrows b
    cols = M.ncols b
    squares = M.toList b

boardSize :: MBoard s -> Int
boardSize b
  | V.length b == 16 = 4
  | V.length b == 25 = 5
  | V.length b == 36 = 6
  | V.length b == 49 = 7
  | V.length b == 64 = 8
  | otherwise = 0

posToIndex :: MBoard s -> B.Position -> Int
posToIndex b (B.Position (x, y)) = x + y * boardSize b

indexToPos :: MBoard s -> Int -> B.Position
indexToPos b i = B.Position (i `mod` boardSize b, i `div` boardSize b)

readSquare :: MBoard s -> B.Position -> IO B.Square
readSquare b p = V.read b (posToIndex b p)

writeSquare :: MBoard s -> B.Position -> B.Square -> IO ()
writeSquare b p s = V.write b (posToIndex b p) s

toBoard :: MBoard s -> IO B.Board
toBoard b = do
  let size = boardSize b
  squares <-
    forM [0 .. (size * size - 1)] $ \i -> do
      readSquare b (indexToPos b i)
  return $ M.fromList size size squares

checkMove :: MBoard s -> B.Move -> IO (Either Moves.InvalidMove Bool)
checkMove b' m = do
  b <- toBoard b'
  return $ Moves.checkMove b m

makeMove :: MBoard s -> B.Move -> IO (Either Moves.InvalidMove ())
makeMove b m@(B.PlaceFlat (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Flat]
      return $ Right ()
makeMove b m@(B.PlaceStanding (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Standing]
      return $ Right ()
makeMove b m@(B.PlaceCap (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Cap]
      return $ Right ()
