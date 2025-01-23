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
  { pc :: Color
  , ps :: Stone
  } deriving (Show, Eq)

data Reserves = Reserves
  { stones :: Int
  , caps :: Int
  } deriving (Show, Eq)

type Stack = [Piece]

type Square = Stack

data Position =
  Position Int Int
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
  | Slide (Position, Direction, [Int], Color)
  deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , turn :: Color
  , moveNumber :: Int
  , player1 :: Reserves
  , player2 :: Reserves
  , result :: Maybe Result
  , history :: [Move]
  } deriving (Show, Eq)

--------------------------
-- | Check Game State | --
--------------------------
checkGameResult :: GameState -> Maybe Result
checkGameResult gs =
  case checkReservesDraw (player1 gs) (player2 gs) of
    Just r -> Just r
    Nothing ->
      case checkFullBoard (board gs) of
        Just r -> Just r
        Nothing -> checkGameWin (board gs)

checkFullBoard :: Board -> Maybe Result
checkFullBoard b = go 1 1 (0, 0)
  where
    n = nrows b
    m = ncols b
    go :: Int -> Int -> (Int, Int) -> Maybe Result
    go x y c@(wc, bc)
      | x > n = go 1 (y + 1) c
      | y >= m =
        if wc > bc
          then Just (Win White)
          else if bc > wc
                 then Just (Win Black)
                 else Just Draw
      | null (getElem x y b) = Nothing
      | otherwise = go (x + 1) y $ addCount c
      where
        addCount :: (Int, Int) -> (Int, Int)
        addCount (wc, bc)
          | pc (head (getElem x y b)) == White = (wc + 1, bc)
          | pc (head (getElem x y b)) == Black = (wc, bc + 1)
          | otherwise = (wc, bc)

checkReservesDraw :: Reserves -> Reserves -> Maybe Result
checkReservesDraw (Reserves 0 0) _ = Just Draw
checkReservesDraw _ (Reserves 0 0) = Just Draw
checkReservesDraw _ _ = Nothing

checkGameWin :: Board -> Maybe Result
checkGameWin b
  | any (findRoad b White) [Position x 1 | x <- [1 .. ncols b]] =
    Just (Win White)
  | any (findRoad (transpose b) White) [Position x 1 | x <- [1 .. nrows b]] =
    Just (Win White)
  | any (findRoad b Black) [Position 1 x | x <- [1 .. nrows b]] =
    Just (Win Black)
  | any (findRoad (transpose b) Black) [Position 1 x | x <- [1 .. ncols b]] =
    Just (Win Black)
  | otherwise = Nothing

-- depth first search with memory of past nodes
findRoad :: Board -> Color -> Position -> Bool
findRoad b c p = go [p] [p]
  where
    n = nrows b
    checkValid :: Position -> [Position] -> Bool
    checkValid pos@(Position x y) visited
      | x < 1 || x > n || y < 1 || y > n = False
      | null (getElem x y b) = False
      | pc (head (getElem x y b)) /= c = False
      | ps (head (getElem x y b)) == Standing = False
      | pos `elem` visited = False
      | otherwise = True
    go :: [Position] -> [Position] -> Bool
    go [] _ = False
    go ((Position i j):xs) visited
      | i == n = True
      | otherwise =
        let neighbors =
              [ Position (i + 1) j -- Down
              , Position i (j - 1) -- Left
              , Position i (j + 1) -- Right
              ]
            validNeighbors = filter (`checkValid` visited) neighbors
         in go (xs ++ validNeighbors) (visited ++ validNeighbors)

--------------------------
-- | Helper Functions | --
--------------------------
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

-------------------------
-- | Print Functions | --
-------------------------
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

letterToCol :: Char -> Int
letterToCol c = fromEnum c - fromEnum 'A' + 1

colToLetter :: Int -> Char
colToLetter n = toEnum (fromEnum 'A' + n - 1)

showSquare :: Square -> (String, Maybe String)
showSquare [] = ("_", Nothing)
showSquare [p] = (pieceString p, Nothing)
showSquare stack =
  let letter = [toEnum (fromEnum 'A' + stackCount)]
      stackCount = stackCounter
      stackCounter = length stack - 1
   in (letter, Just $ letter ++ ": " ++ stackString stack)

stacksWithKeys :: [(String, Stack)]
stacksWithKeys = []

boardString :: Board -> String
boardString b =
  unlines $
  [show (n - i) ++ " |" ++ row i | i <- [1 .. n]] ++
  ["  " ++ concat ["  " ++ [colToLetter j] ++ "  " | j <- [1 .. m]]] ++
  (if not (null keys)
     then "Key:" : keys
     else [])
  where
    n = nrows b
    m = ncols b
    row i = concat [" " ++ padSquare (square (i + 1) j) ++ " |" | j <- [1 .. m]]
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
