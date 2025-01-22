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

data Piece = Piece {pc :: Color, ps :: Stone }

data Bag = Bag {stones :: Int, caps :: Int}

type Stack = [Piece]

type Square = Stack

data Position = Position Int Int deriving (Show, Eq)

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
    checkValid p@(Position x y) xs
      | x < 0 || x > n || y > m = False
      | null (getElem x y b) = False
      | pc (head (getElem x y b)) /= c = False
      | ps (head (getElem x y b)) == Standing = False
      | p `elem` xs = False
      | otherwise = True
    go :: [Position] -> [Position] -> Bool
    go ((Position _ m):_) _ = True
    go (x@(Position i j):xs) ys
      | ab && bb && cb = go xs (a : b : c : ys)
      | ab && bb = go xs (a : b : ys)
      | ab && cb = go xs (a : c : ys)
      | bb && cb = go xs (b : c : ys)
      | ab = go xs (a : ys)
      | bb = go xs (b : ys)
      | cb = go xs (c : ys)
      | otherwise = go xs ys
      where
        a = Position (i - 1) j
        b = Position (i + 1) j
        c = Position i (j + 1)
        ab = checkValid a ys
        bb = checkValid b ys
        cb = checkValid c ys

