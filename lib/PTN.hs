module PTN where

import qualified Board as B
import Data.Char (digitToInt, isDigit, isLetter)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

data PTN = PTN
  { site :: String
  , event :: String
  , date :: String
  , time :: String
  , p1 :: String
  , p2 :: String
  , clock :: String
  , ptnResult :: String
  , size :: Int
  , moves :: B.History
  } deriving (Show)

data PTNParseError =
  PTNParseError
  deriving (Show, Eq)

parsePTN :: Text -> Either PTNParseError PTN
parsePTN input = do
  let lines = T.lines input
  site <- extractField "Site" lines
  event <- extractField "Event" lines
  date <- extractField "Date" lines
  time <- extractField "Time" lines
  p1 <- extractField "Player1" lines
  p2 <- extractField "Player2" lines
  clock <- extractField "Clock" lines
  ptnResult <- extractField "Result" lines
  sizeStr <- extractField "Size" lines
  moves <- parseMoves (map (T.dropWhile (/= ' ') . T.strip) lines)
  return
    PTN
      { site = site
      , event = event
      , date = date
      , time = time
      , p1 = p1
      , p2 = p2
      , clock = clock
      , ptnResult = ptnResult
      , size = read sizeStr :: Int
      , moves = moves
      }

parseMoves :: [Text] -> Either PTNParseError B.History
parseMoves moves = traverse parseMove (filter (not . T.null) moves)

parseMove :: Text -> Either PTNParseError B.Move
parseMove move =
  case T.unpack move of
    [col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceFlat (B.Position (digitToInt row) (B.letterToCol col), B.White)
    ['!', col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceStanding
          (B.Position (digitToInt row) (B.letterToCol col), B.White)
    ['+', col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceCap (B.Position (digitToInt row) (B.letterToCol col), B.White)
    (col:row:rest)
      | isLetter col && isDigit row -> parseSlideMove (col : row : rest)
    _ -> Left PTNParseError

parseSlideMove :: String -> Either PTNParseError B.Move
parseSlideMove (col:row:rest) =
  let pos = B.Position (digitToInt row) (B.letterToCol col)
      (countStr, dirChar:_) = span isDigit rest
      count = read countStr
      dir =
        case dirChar of
          '+' -> B.Up
          '-' -> B.Down
          '>' -> B.Right
          '<' -> B.Left
   in Right $ B.Slide (pos, count, dir, [], B.White, False)
parseSlideMove _ = Left PTNParseError

extractField :: String -> [Text] -> Either PTNParseError String
extractField fieldName lines =
  case find (T.isPrefixOf (T.pack ("[" ++ fieldName ++ ": "))) lines of
    Just line -> Right $ T.unpack $ T.drop (length fieldName + 4) line
    Nothing -> Left PTNParseError
