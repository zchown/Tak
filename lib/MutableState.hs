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
