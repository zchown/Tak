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

data Piece = Piece 
  {pc :: Color, ps :: Stone} 
  deriving (Show, Eq)

data Bag = Bag 
  {stones :: Int, caps :: Int} 
  deriving (Show, Eq)

type Stack = [Piece]

type Square = Stack

data Position = Position Int Int 
  deriving (Show, Eq)

type Board = Matrix Square

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

data Result
  = Win Color
  | Draw
  deriving (Show, Eq)

data Move
  = PlaceFlat (Position, Color)
  | PlaceStanding (Position, Color)
  | PlaceCap (Position, Color)
  | Slide (Direction, [Int], Color)
  deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , turn :: Color
  , player1 :: Bag
  , player2 :: Bag
  , result :: Maybe Result
  , history :: [Move]
  } deriving (Show, Eq)

--------------------------
-- | Check Game State | --
--------------------------

checkGameResult :: GameState -> Maybe Result
checkGameResult gs = case checkGameWin (board gs) of
  Just r -> Just r
  Nothing -> case checkFullDraw (board gs) of
    Just r -> Just r
    Nothing -> checkBagDraw (player1 gs) (player2 gs)

checkFullDraw :: Board -> Maybe Result
checkFullDraw b = go 0 0
  where
    n = nrows b
    m = ncols b
    go :: Int -> Int -> Maybe Result
    go x y
      | x >= n = go 0 (y + 1)
      | y >= m = Just Draw
      | null (getElem (x + 1) (y + 1) b) = Nothing
      | otherwise = go (x + 1) y

checkBagDraw :: Bag -> Bag -> Maybe Result
checkBagDraw (Bag 0 0) _ = Just Draw
checkBagDraw _ (Bag 0 0) = Just Draw
checkBagDraw _ _ = Nothing

checkGameWin :: Board -> Maybe Result
checkGameWin b
  | any (findRoad b White) [Position x y | x <- [1..nrows b], y <- [1..ncols b]] = Just (Win White)
  | any (findRoad b Black) [Position x y | x <- [1..nrows b], y <- [1..ncols b]] = Just (Win Black)
  | otherwise = Nothing

-- depth first search with memory of past nodes
findRoad :: Board -> Color -> Position -> Bool
findRoad b c p = go [p] [p]
  where
    n = nrows b
    m = ncols b
    checkValid :: Position -> [Position] -> Bool
    checkValid pos@(Position x y) visited
      | x < 1 || x > n || y < 1 || y > m = False
      | null (getElem x y b) = False
      | pc (head (getElem x y b)) /= c = False
      | ps (head (getElem x y b)) == Standing = False
      | pos `elem` visited = False
      | otherwise = True
    go :: [Position] -> [Position] -> Bool
    go [] _ = False
    go ((Position i j):xs) visited
      | (c == White && j == m) || (c == Black && i == n) = True
      | otherwise = 
          let neighbors = [
                Position (i - 1) j,  -- Up
                Position (i + 1) j,  -- Down
                Position i (j - 1),  -- Left
                Position i (j + 1)   -- Right
                ]
              validNeighbors = filter (`checkValid` visited) neighbors
          in go (xs ++ validNeighbors) (visited ++ validNeighbors)


--------------------------
-- | Helper Functions | --
--------------------------
createEmptyBoard :: Int -> Board
createEmptyBoard size = matrix size size (const [])
