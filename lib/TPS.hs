{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TPS where

import Board
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data ParseError
  = InvalidPiece Char
  | InvalidBoardSize Int
  | InvalidTPSFormat Text
  | InvalidSquareFormat Text
  | InvalidMoveNumber
  deriving (Show, Eq)

parseTPS :: Text -> Either ParseError GameState
parseTPS t = do
  case (splitTPS . cleanTPS) t of
    Prelude.Right [boardStr, turnStr, moveNumberStr] -> do
      case parseMoveNumber moveNumberStr of
        Prelude.Left e -> Prelude.Left e
        Prelude.Right mn -> do
          let n = length $ T.splitOn "/" boardStr
          case parseBoard boardStr n of
            Prelude.Left e -> Prelude.Left e
            Prelude.Right b -> do
              return $
                GameState
                  { board = b
                  , turn = parseTurn turnStr
                  , moveNumber = mn
                  , player1 = getReserves b White
                  , player2 = getReserves b Black
                  , result = Nothing
                  , gameHistory = []
                  }
    _ -> Prelude.Left $ InvalidTPSFormat t

-- useful for testing when I know the input is correct
parseTPSHard :: Text -> GameState
parseTPSHard t =
  case parseTPS t of
    Prelude.Left e -> error $ show e
    Prelude.Right gs -> gs

cleanTPS :: Text -> Text
cleanTPS =
  T.strip .
  T.replace "[TPS " "" .
  T.replace "]" "" . T.replace "x," "x1," . T.replace "x/" "x1/"

splitTPS :: Text -> Either ParseError [Text]
splitTPS t =
  case T.splitOn " " t of
    [boardStr, turnStr, moveNumberStr] ->
      Prelude.Right [boardStr, turnStr, moveNumberStr]
    _ -> Prelude.Left $ InvalidTPSFormat t

parseTurn :: Text -> Color
parseTurn "1" = White
parseTurn _ = Black

parseMoveNumber :: Text -> Either ParseError Int
parseMoveNumber t =
  case (readMaybe . T.unpack) t of
    Nothing -> Prelude.Left InvalidMoveNumber
    Just i -> Prelude.Right i

parseBoard :: Text -> Int -> Either ParseError Board
parseBoard boardStr n = do
  rows <- mapM (parseRow n) $ T.splitOn "/" boardStr
  let cr = concat rows
  if length cr /= n * n
    then Prelude.Left $ InvalidBoardSize n
    else Prelude.Right $ fromList n n cr

parseRow :: Int -> Text -> Either ParseError [Square]
parseRow n row =
  let r = concat <$> mapM parseSquare (T.splitOn "," row)
   in case r of
        Prelude.Right r' ->
          if length r' /= n
            then Prelude.Left $ InvalidBoardSize n
            else Prelude.Right r'
        l -> l

parseSquare :: Text -> Either ParseError [Square]
parseSquare square =
  case T.unpack square of
    [] -> Prelude.Right []
    'x':y -> Prelude.Right $ replicate (read y) []
    xs -> (: []) <$> parseSingleSquare xs []

parseSingleSquare :: [Char] -> Stack -> Either ParseError Square
parseSingleSquare [] acc = Prelude.Right acc
parseSingleSquare (x:xs) acc@(y:ys) =
  case x of
    '1' -> parseSingleSquare xs (Piece White Flat : acc)
    '2' -> parseSingleSquare xs (Piece Black Flat : acc)
    'S' -> parseSingleSquare xs (modifyStanding y : ys)
    'C' -> parseSingleSquare xs (modifyCap y : ys)
    _ -> Prelude.Left $ InvalidPiece x
  where
    modifyStanding (Piece c _) = Piece c Standing
    modifyCap (Piece c _) = Piece c Cap
parseSingleSquare (x:xs) [] =
  case x of
    '1' -> parseSingleSquare xs [Piece White Flat]
    '2' -> parseSingleSquare xs [Piece Black Flat]
    _ -> Prelude.Left $ InvalidPiece x

getReserves :: Board -> Color -> Reserves
getReserves b c
  | ncols b == 4 = Reserves (15 - x) 0
  | ncols b == 5 = Reserves (21 - x) (1 - y)
  | ncols b == 6 = Reserves (30 - x) (1 - y)
  | ncols b == 7 = Reserves (40 - x) (2 - y)
  | ncols b == 8 = Reserves (50 - x) (2 - y)
  | otherwise = Reserves 0 0
  where
    (Reserves x y) = getPlaced b c

--------------------------
-- | GameState to TPS | --
--------------------------
gameStateToTPS :: GameState -> Text
gameStateToTPS gs =
  T.concat
    [ boardToTPS (board gs)
    , " "
    , turnToTPS (turn gs)
    , " "
    , T.pack $ show (moveNumber gs)
    ]

boardToTPS :: Board -> Text
boardToTPS b = T.intercalate "/" $ map rowToTPS (toLists b)

rowToTPS :: [Square] -> Text
rowToTPS row = T.intercalate "," $ foldr groupSquares [] row
  where
    groupSquares :: Square -> [Text] -> [Text]
    groupSquares [] [] = ["x1"]
    groupSquares [] (x:xs)
      | T.take 1 x == "x" =
        T.concat ["x", T.pack $ show (1 + read @Int (T.unpack $ T.drop 1 x))] : xs
      | otherwise = "x1" : x : xs
    groupSquares square acc = squareToTPS square : acc

squareToTPS :: Square -> Text
squareToTPS [] = "x"
squareToTPS stack =
  T.pack $ foldr (\piece acc -> pieceToChar piece : acc) "" stack
  where
    pieceToChar :: Piece -> Char
    pieceToChar (Piece White Flat) = '1'
    pieceToChar (Piece Black Flat) = '2'
    pieceToChar (Piece White Standing) = 'S'
    pieceToChar (Piece Black Standing) = 'S'
    pieceToChar (Piece White Cap) = 'C'
    pieceToChar (Piece Black Cap) = 'C'

turnToTPS :: Color -> Text
turnToTPS White = "1"
turnToTPS Black = "2"
