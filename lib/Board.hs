{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isLetter, toLower)
import Data.Matrix
import GHC.Generics (Generic)

data Stone
  = Flat
  | Standing
  | Cap
  deriving (Show, Eq, Generic)

instance ToJSON Stone

instance FromJSON Stone

data Color
  = White
  | Black
  deriving (Show, Eq, Generic)

instance ToJSON Color

instance FromJSON Color

data Piece = Piece
  { pc :: Color
  , ps :: Stone
  } deriving (Show, Eq, Generic)

instance ToJSON Piece

instance FromJSON Piece

data Reserves = Reserves
  { stones :: Int
  , caps :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Reserves

instance FromJSON Reserves

type Stack = [Piece]

type Square = Stack

newtype Position =
  Position (Int, Int)
  deriving (Show, Eq, Generic)

instance ToJSON Position

instance FromJSON Position

type Board = Matrix Square

instance ToJSON Board

instance FromJSON Board

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq, Generic)

instance ToJSON Direction

instance FromJSON Direction

data Result
  = Road Color
  | FlatWin Color
  | Draw
  | Continue
  deriving (Show, Eq, Generic)

instance ToJSON Result

instance FromJSON Result

type Crush = Bool

type Count = Int

data Move
  = PlaceFlat (Position, Color)
  | PlaceStanding (Position, Color)
  | PlaceCap (Position, Color)
  | Slide (Position, Count, Direction, [Int], Color, Crush)
  deriving (Show, Eq, Generic)

instance ToJSON Move

instance FromJSON Move

type History = [Move]

data GameState = GameState
  { board :: Board
  , turn :: Color
  , moveNumber :: Int
  , player1 :: Reserves
  , player2 :: Reserves
  , result :: Result
  , gameHistory :: History
  } deriving (Show, Eq, Generic)

instance ToJSON GameState

instance FromJSON GameState

--------------------------
-- | Check Game State | --
--------------------------
checkGameResult :: GameState -> Result
checkGameResult gs =
  case checkGameWin (board gs) of
    Continue ->
      case checkReservesDraw (player1 gs) (player2 gs) of
        Nothing ->
          case checkFullBoard (board gs) True of
            Continue -> Draw
            x -> x
        _ -> checkFullBoard (board gs) False
    r -> r

checkFullBoard :: Board -> Bool -> Result
checkFullBoard b f = go 1 1 (0, 0)
  where
    n = nrows b
    m = ncols b
    go :: Int -> Int -> (Int, Int) -> Result
    go x y c@(wc, bc)
      | x > n = go 1 (y + 1) c
      | y >= m =
        if wc > bc
          then FlatWin White
          else if bc > wc
                 then FlatWin Black
                 else Draw
      | not f && null (getElem x y b) = Continue
      | null (getElem x y b) = go (x + 1) y c
      | otherwise = go (x + 1) y $ addCount c
      where
        addCount :: (Int, Int) -> (Int, Int)
        addCount (wc', bc')
          | pc (head (getElem x y b)) == White = (wc' + 1, bc')
          | pc (head (getElem x y b)) == Black = (wc', bc' + 1)
          | otherwise = (wc, bc)

checkReservesDraw :: Reserves -> Reserves -> Maybe Result
checkReservesDraw (Reserves 0 0) _ = Nothing
checkReservesDraw _ (Reserves 0 0) = Nothing
checkReservesDraw _ _ = Just Continue

checkGameWin :: Board -> Result
checkGameWin b
  | any
      (findRoad b White)
      (filter (validPos White) [Position (x, 1) | x <- [1 .. ncols b]]) =
    Road White
  | any
      (findRoad (transpose b) White)
      (filter (validPos White) [Position (x, 1) | x <- [1 .. nrows b]]) =
    Road White
  | any
      (findRoad b Black)
      (filter (validPos Black) [Position (x, 1) | x <- [1 .. nrows b]]) =
    Road Black
  | any
      (findRoad (transpose b) Black)
      (filter (validPos Black) [Position (x, 1) | x <- [1 .. nrows b]]) =
    Road Black
  | otherwise = Continue
  where
    validPos :: Color -> Position -> Bool
    validPos c (Position (x, y)) =
      case getElem x y b of
        [] -> False
        p:_ -> (pc p == c) && (ps p /= Standing)

findRoad :: Board -> Color -> Position -> Bool
findRoad b c startPos = go [startPos] []
  where
    n = nrows b
    checkValid :: Position -> [Position] -> Bool
    checkValid pos@(Position (x, y)) visited
      | x < 1 || x > n || y < 1 || y > n = False
      | null (getElem x y b) = False
      | pc (head (getElem x y b)) /= c = False
      | ps (head (getElem x y b)) == Standing = False
      | pos `elem` visited = False
      | otherwise = True
    go :: [Position] -> [Position] -> Bool
    go [] _ = False
    go (pos@(Position (i, j)):stack) visited
      | j == n = True
      | otherwise =
        let neighbors =
              [ Position (i, j + 1) -- Up
              , Position (i, j - 1) -- Down
              , Position (i - 1, j) -- Left
              , Position (i + 1, j) -- Right
              ]
            validNeighbors = filter (`checkValid` visited) neighbors
            newStack = stack ++ validNeighbors
            newVisited = pos : visited
         in Position (i, n) `elem` validNeighbors || go newStack newVisited

-- --------------------------
-- -- | Helper Functions | --
-- --------------------------
createEmptyBoard :: Int -> Board
createEmptyBoard size = matrix size size (const [])

getTopPiece :: Int -> Int -> Board -> Maybe Piece
getTopPiece i j b =
  case getElem i j b of
    [] -> Nothing
    x:_ -> Just x

addToStack :: Stack -> Piece -> Maybe Stack
addToStack [] p = Just [p]
addToStack xs@(x:_) p
  | ps x == Flat = Just (p : xs)
  | otherwise = Nothing

getAllPieces :: Board -> Color -> [Piece]
getAllPieces b c = concatMap (filter (\x -> pc x == c)) (toList b)

getPlaced :: Board -> Color -> Reserves
getPlaced b c =
  Reserves
    { stones = length (filter (\x -> ps x == Flat || ps x == Standing) ap)
    , caps = length (filter (\x -> ps x == Cap) ap)
    }
  where
    ap = getAllPieces b c

letterToCol :: Char -> Int
letterToCol c
  | not (isLetter c) = error $ "Invalid column letter: " ++ [c]
  | otherwise = fromEnum (toLower c) - fromEnum 'a' + 1

colToLetter :: Int -> Char
colToLetter n = toEnum (fromEnum 'a' + n - 1)

placeFlat :: Board -> Position -> Color -> Board
placeFlat b (Position (x, y)) c = setElem (Piece c Flat : xs) (x, y) b
  where
    xs = getElem x y b

placeStanding :: Board -> Position -> Color -> Board
placeStanding b (Position (x, y)) c = setElem [Piece c Standing] (x, y) b

placeCap :: Board -> Position -> Color -> Board
placeCap b (Position (x, y)) c = setElem [Piece c Cap] (x, y) b

getInverseDir :: Direction -> Direction
getInverseDir Up = Down
getInverseDir Down = Up
getInverseDir Board.Left = Board.Right
getInverseDir Board.Right = Board.Left

getNextPos :: Position -> Direction -> (Position, Int, Int)
getNextPos (Position (x, y)) Up = (Position (x, y + 1), x, y + 1)
getNextPos (Position (x, y)) Down = (Position (x, y - 1), x, y - 1)
getNextPos (Position (x, y)) Board.Left = (Position (x - 1, y), x - 1, y)
getNextPos (Position (x, y)) Board.Right = (Position (x + 1, y), x + 1, y)

getSlidePos :: Position -> Direction -> Int -> (Position, Int, Int)
getSlidePos (Position (x, y)) Up n = (Position (x, y + n), x, y + n)
getSlidePos (Position (x, y)) Down n = (Position (x, y - n), x, y - n)
getSlidePos (Position (x, y)) Board.Left n = (Position (x - n, y), x - n, y)
getSlidePos (Position (x, y)) Board.Right n = (Position (x + n, y), x + n, y)

getMoveColor :: Move -> Color
getMoveColor (PlaceFlat (_, c)) = c
getMoveColor (PlaceStanding (_, c)) = c
getMoveColor (PlaceCap (_, c)) = c
getMoveColor (Slide (_, _, _, _, c, _)) = c

flipMoveColor :: Move -> Move
flipMoveColor (PlaceFlat (p, c)) = PlaceFlat (p, flipColor c)
flipMoveColor (PlaceStanding (p, c)) = PlaceStanding (p, flipColor c)
flipMoveColor (PlaceCap (p, c)) = PlaceCap (p, flipColor c)
flipMoveColor (Slide (p, n, d, ds, c, crush)) =
  Slide (p, n, d, ds, flipColor c, crush)

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

flipStanding :: Piece -> Piece
flipStanding (Piece c Standing) = Piece c Flat
flipStanding (Piece c Flat) = Piece c Standing
flipStanding p = p

getNewReserves :: Reserves -> Reserves -> Move -> (Reserves, Reserves)
getNewReserves (Reserves s1 c1) p2 (PlaceFlat (_, White)) =
  (Reserves (s1 - 1) c1, p2)
getNewReserves p1 (Reserves s2 c2) (PlaceFlat (_, Black)) =
  (p1, Reserves (s2 - 1) c2)
getNewReserves (Reserves s1 c1) p2 (PlaceStanding (_, White)) =
  (Reserves (s1 - 1) c1, p2)
getNewReserves p1 (Reserves s2 c2) (PlaceStanding (_, Black)) =
  (p1, Reserves (s2 - 1) c2)
getNewReserves (Reserves s1 c1) p2 (PlaceCap (_, White)) =
  (Reserves s1 (c1 - 1), p2)
getNewReserves p1 (Reserves s2 c2) (PlaceCap (_, Black)) =
  (p1, Reserves s2 (c2 - 1))
getNewReserves p1 p2 _ = (p1, p2)

hasReserves :: Reserves -> Reserves -> Move -> Bool
hasReserves (Reserves s _) _ (PlaceFlat (_, White)) = s > 0
hasReserves _ (Reserves s _) (PlaceFlat (_, Black)) = s > 0
hasReserves (Reserves s _) _ (PlaceStanding (_, White)) = s > 0
hasReserves _ (Reserves s _) (PlaceStanding (_, Black)) = s > 0
hasReserves (Reserves _ c) _ (PlaceCap (_, White)) = c > 0
hasReserves _ (Reserves _ c) (PlaceCap (_, Black)) = c > 0
hasReserves _ _ _ = True

-- -------------------------
-- -- | Print Functions | --
-- -------------------------
pieceString :: Piece -> String
pieceString (Piece White Flat) = "1"
pieceString (Piece White Standing) = "1S"
pieceString (Piece White Cap) = "1C"
pieceString (Piece Black Flat) = "2"
pieceString (Piece Black Standing) = "2S"
pieceString (Piece Black Cap) = "2C"

stackString :: Stack -> String
stackString [] = "[]"
stackString xs = (concatMap (\x -> pieceString x ++ " ") . reverse) xs

colorString :: Color -> String
colorString White = "White"
colorString Black = "Black"

stringColor :: String -> Color
stringColor "White" = White
stringColor _ = Black

showSquare :: Square -> (String, Maybe String)
showSquare [] = ("_", Nothing)
showSquare [p] = (pieceString p, Nothing)
showSquare stack =
  let letter = [toEnum (fromEnum 'a' + stackCount)]
      stackCount = stackCounter
      stackCounter = length stack - 1
   in (letter, Just $ letter ++ ": " ++ stackString stack)

boardString :: Board -> String
boardString b' =
  unlines $
  [show (n - i + 1) ++ " |" ++ row i | i <- [1 .. n]] ++
  [" " ++ concat ["    " ++ [colToLetter j] ++ "  " | j <- [1 .. m]]] ++
  (if not (null keys)
     then "Key:" : keys
     else [])
  where
    b = transpose b'
    n = nrows b
    m = ncols b
    row i =
      concat [" " ++ padSquare (square (n - i + 1) j) ++ " |" | j <- [1 .. m]]
    square i j =
      let (display, _) = showSquare (getElem i j b)
       in display
    padSquare s =
      let pad = (3 - length s) `div` 2
          extraPad =
            if even (length s)
              then 0
              else 1
       in replicate pad ' ' ++ s ++ replicate (pad + extraPad) ' '
    allSquares = [showSquare (getElem i j b) | i <- [1 .. n], j <- [1 .. m]]
    keys = [k | (_, Just k) <- allSquares]
