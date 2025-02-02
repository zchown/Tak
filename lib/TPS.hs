{-# LANGUAGE OverloadedStrings #-}

module TPS where

import Board
import qualified Data.List as L
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
      mn <- parseMoveNumber moveNumberStr
      let boardSize = length $ T.splitOn "/" boardStr
      b <- parseBoard boardStr boardSize
      return $
        GameState
          { board = b
          , turn = parseTurn turnStr
          , moveNumber = mn
          , player1 = getReserves b White
          , player2 = getReserves b Black
          , result = Continue
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
  -- the x, and x/ are useful to make parsing easier
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
  let cr = concat . L.transpose . reverse $ rows
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
boardToTPS b =
  T.intercalate "/" $ map rowToTPS ((reverse . L.transpose . toLists) b)
  where
    rowToTPS :: [Square] -> Text
    rowToTPS row = T.intercalate "," $ compressXs $ map squareToTPS row
    compressXs :: [Text] -> [Text]
    compressXs = concatMap compressGroup . L.group
      where
        compressGroup xs@(x:_)
          | x == "x" && length xs > 1 =
            [T.concat [x, T.pack $ show (length xs)]]
          | otherwise = xs
        compressGroup [] = []

squareToTPS :: Square -> Text
squareToTPS [] = "x"
squareToTPS stack = T.concat $ map pieceToTPS (reverse stack)

pieceToTPS :: Piece -> Text
pieceToTPS (Piece White Flat) = "1"
pieceToTPS (Piece Black Flat) = "2"
pieceToTPS (Piece White Standing) = "1S"
pieceToTPS (Piece Black Standing) = "2S"
pieceToTPS (Piece White Cap) = "1C"
pieceToTPS (Piece Black Cap) = "2C"

turnToTPS :: Color -> Text
turnToTPS White = "1"
turnToTPS Black = "2"
