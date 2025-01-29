{-# LANGUAGE OverloadedStrings #-}

module TPS where

import Board
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as T

parseTPS :: Text -> GameState
parseTPS t =
  GameState
    { board = b
    , turn =
        if turnStr == "1"
          then White
          else Black
    , moveNumber = read (T.unpack moveNumberStr) :: Int
    , player1 = getReserves b White
    , player2 = getReserves b Black
    , result = Nothing
    , gameHistory = []
    }
  where
    -- Remove "[TPS ]" prefix and suffix if present
    -- also adds a 1 to single x's to make parsing easier
    cleanedT =
      T.strip $
      T.replace "[TPS " "" $
      T.replace "]" "" $ T.replace "x," "x1," $ T.replace "x/" "x1/" t
    [boardStr, turnStr, moveNumberStr] = T.splitOn " " cleanedT
    n = length $ T.splitOn "/" boardStr
    b = parseBoard boardStr n

parseBoard :: Text -> Int -> Board
parseBoard boardStr n =
  fromList n n $ concatMap parseRow $ T.splitOn "/" boardStr
  where
    parseRow :: Text -> [Square]
    parseRow row = concatMap parseSquare $ T.splitOn "," row

parseSquare :: Text -> [Square]
parseSquare square =
  case T.unpack square of
    [] -> []
    'x':y -> replicate (read y :: Int) []
    xs -> [parseSingleSquare xs []]
  where
    parseSingleSquare :: [Char] -> Stack -> Square
    parseSingleSquare [] acc = acc
    parseSingleSquare (x:xs) acc@(y:ys) =
      case x of
        '1' -> parseSingleSquare xs (Piece White Flat : acc)
        '2' -> parseSingleSquare xs (Piece Black Flat : acc)
        'S' ->
          parseSingleSquare
            xs
            (if pc y == White
               then Piece White Standing : ys
               else Piece Black Standing : ys)
        'C' ->
          parseSingleSquare
            xs
            (if pc y == White
               then Piece White Cap : ys
               else Piece Black Cap : ys)
        _ -> error "Invalid piece"
    parseSingleSquare (x:xs) [] =
      case x of
        '1' -> parseSingleSquare xs [Piece White Flat]
        '2' -> parseSingleSquare xs [Piece Black Flat]
        _ -> error "Invalid piece"

getReserves :: Board -> Color -> Reserves
getReserves b c
  | ncols b == 4 = Reserves (15 - x) 0
  | ncols b == 5 = Reserves (21 - x) (1 - y)
  | ncols b == 6 = Reserves (30 - x) (1 - y)
  | ncols b == 7 = Reserves (40 - x) (2 - y)
  | ncols b == 8 = Reserves (50 - x) (2 - y)
  | otherwise = error "Invalid board size"
  where
    (Reserves x y) = getPlaced b c
