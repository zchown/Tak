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

data Piece = Piece Color Stone

getColor :: Piece -> Color
getColor (Piece (c, _)) = c

getStone :: Piece -> Stone
getStone (Piece (_, s)) = s

data Bag = Bag {stones :: Int, caps :: Int}

type Stack = [Piece]

type Square = Stack

type Position = (Int, Int)

type Board = Matrix Square

data Direction = Up | Down | Left | Right

data Result
  = Win Color
  | Draw

data Move
  = PlaceFlat (Position, Color)
  | PlaceStanding (Position, Color)
  | PlaceCap (Position, Color)
  | Slide (Direction, [Int], Color)

data GameState = GameState
  { board :: Board
  , turn :: Color
  , player1 :: Bag
  , player2 :: Bag
  , result :: Maybe Result
  , history :: [Move]
  }

--------------------------
-- | Check Game State | --
--------------------------

checkGameResult :: GameState -> Result
checkGameResult gs = undefined

checkFullDraw :: Board -> Maybe Result
checkFullDraw b = go 0 0
  where
  n = nrows b
  m = ncols b
  go :: Int -> Int -> Maybe Result
  go x y
    | x >= n = go 0 (y + 1)
    | y >= m = Just Draw
    | null (getElem x y b) = Nothing
    | otherwise = go (x + 1) y

checkBagDraw :: Bag -> Bag -> Maybe Result
checkBagDraw (Bag 0 0) _ = Just Draw
checkBagDraw _ (Bag 0 0) = Just Draw
checkBagDraw _ _ = Nothing

checkGameWin :: Board -> Maybe Result
checkGameWin b = undefined

-- findRoad :: Board -> Color -> Bool
-- findRoad :: 

-- depth first search
-- position should be in the top row 
-- handle matrix manipulation for this to happen in find road
searchRoad :: Board -> Color -> Position -> Bool
searchRoad b c p = go [p] [p]
  where
    n = nrows b
    m = ncols b
    checkValid :: Position -> [Position] -> Bool
    checkValid p@(x, y) xs
      | null (getElem x y b) = False
      | getColor (head (getElem x y b)) /= c = False
      | getStone (head (getElem x y b)) == Standing = False
      | p `elem` xs = False
      | otherwise = True
    go :: [Position] -> [Position] -> Bool
    go ((_, m):_) _ = True
    go (x@(0, y):xs) ys = go (if (checkValid (Position (1, y)) ys) then (Position (1, y)) else []) : (if checkValid (Position (0, y - 1)) then (Position (0, y - 1)) else []) : ys

     

