{-# LANGUAGE OverloadedStrings #-}

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
  deriving (Show)

parseTPS :: Text -> Either ParseError GameState
parseTPS t = do
  let cleanedT = cleanTPS t
  case splitTPS cleanedT of
    Prelude.Right [boardStr, turnStr, moveNumberStr] -> do
      let tn = parseTurn turnStr
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
                  , turn = tn
                  , moveNumber = mn
                  , player1 = getReserves b White
                  , player2 = getReserves b Black
                  , result = Nothing
                  , gameHistory = []
                  }
    _ -> Prelude.Left $ InvalidTPSFormat $ T.pack "Invalid TPS format"

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
  rows <- mapM parseRow $ T.splitOn "/" boardStr
  return $ fromList n n (concat rows)

parseRow :: Text -> Either ParseError [Square]
parseRow row = concat <$> mapM parseSquare (T.splitOn "," row)

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
