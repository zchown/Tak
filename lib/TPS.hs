module Board where

import Data.Matrix

data Stone
  = Flat
  | Standing
  | Cap
  deriving (Show, Eq)

data Color
  = White
  | Black
  deriving (Show, Eq)

newtype Piece =
  Piece (Color, Stone)
  deriving (Show, Eq)

type Stack = [Piece]

type Square = Stack

type Position = (Int, Int)

type Board = Matrix Square

data Direction = Up | Down | Left | Right

data Move
  = PlaceFlat (Position, Color)
  | PlaceStanding (Position, Color)
  | PlaceCap (Position, Color)
  | Slide (Direction, [Int], Color)

data GameState = GameState
  { board :: Board
  , turn :: Color
  , winner :: Maybe Color
  , history :: [Move]
  }

--------------------------
-- | Check Game State | --
--------------------------
