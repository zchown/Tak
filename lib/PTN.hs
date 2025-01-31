module PTN where

import qualified Board as B
import Data.Char (digitToInt, isDigit, isLetter)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

data PTN = PTN
  { site :: Maybe String
  , event :: Maybe String
  , date :: Maybe String
  , time :: Maybe String
  , p1 :: Maybe String
  , p2 :: Maybe String
  , clock :: Maybe String
  , ptnResult :: Maybe String
  , size :: Maybe Int
  , moves :: B.History
  } deriving (Show, Eq)

data PTNParseError
  = PTNParseError
  | PTNDirectionError
  | PTNCountError
  | PTNMoveError
  | PTNPositionError
  | PTNColorError
  | PTNStoneError
  | PTNSlideError
  deriving (Show, Eq)

parsePTN :: Text -> Either PTNParseError PTN
parsePTN input = do
  let lines' = T.lines input
  site' <- extractField "Site" lines'
  event' <- extractField "Event" lines'
  date' <- extractField "Date" lines'
  time' <- extractField "Time" lines'
  p1' <- extractField "Player1" lines'
  p2' <- extractField "Player2" lines'
  clock' <- extractField "Clock" lines'
  ptnResult' <- extractField "Result" lines'
  sizeStr' <- extractField "Size" lines'
  let size' =
        case sizeStr' of
          Just str -> read <$> Just str
          Nothing -> Nothing
  let moveLines = filter isMoveLine lines'
  moves' <- parseMoves moveLines
  return
    PTN
      { site = site'
      , event = event'
      , date = date'
      , time = time'
      , p1 = p1'
      , p2 = p2'
      , clock = clock'
      , ptnResult = ptnResult'
      , size = size'
      , moves = moves'
      }

isMoveLine :: Text -> Bool
isMoveLine line =
  let trimmed = T.strip line
   in not (T.null trimmed) && T.head trimmed `elem` ['0' .. '9']

parseMoves :: [Text] -> Either PTNParseError B.History
parseMoves ms = do
  movePairs <- traverse parseMovePair ms
  return (concat movePairs)

parseMovePair :: Text -> Either PTNParseError [B.Move]
parseMovePair movePair = do
  let moveStr = T.unpack movePair
  case words moveStr of
    [_, whiteMove] -> do
      whiteMove' <- parseSingleMove whiteMove B.White
      return [whiteMove']
    [_, whiteMove, blackMove] -> do
      whiteMove' <- parseSingleMove whiteMove B.White
      blackMove' <- parseSingleMove blackMove B.Black
      return [whiteMove', blackMove']
    _ -> Left PTNMoveError

parseSingleMove :: String -> B.Color -> Either PTNParseError B.Move
parseSingleMove moveStr color =
  case moveStr of
    [col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceFlat (B.Position (digitToInt row, B.letterToCol col), color)
    ['S', col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceStanding (B.Position (digitToInt row, B.letterToCol col), color)
    ['C', col, row]
      | isLetter col && isDigit row ->
        Right $
        B.PlaceCap (B.Position (digitToInt row, B.letterToCol col), color)
    _ -> parseSlideMove moveStr color

parseSlideMove :: String -> B.Color -> Either PTNParseError B.Move
parseSlideMove str color =
  case str of
    [countChar, col, row, dir] ->
      let pos = B.Position (digitToInt row, B.letterToCol col)
          dir' =
            case dir of
              '+' -> Right B.Up
              '-' -> Right B.Down
              '>' -> Right B.Right
              '<' -> Right B.Left
              _ -> Left PTNDirectionError
          count = digitToInt countChar
       in case dir' of
            Right d -> Right $ B.Slide (pos, count, d, [], color, False)
            Left err -> Left err
    countChar:col:row:dir:dropsStr ->
      let pos = B.Position (digitToInt row, B.letterToCol col)
          dir' =
            case dir of
              '+' -> Right B.Up
              '-' -> Right B.Down
              '>' -> Right B.Right
              '<' -> Right B.Left
              _ -> Left PTNDirectionError
          count = digitToInt countChar
          drops = map digitToInt dropsStr
       in case dir' of
            Right d -> Right $ B.Slide (pos, count, d, drops, color, False)
            Left err -> Left err
    [col, row, dir] ->
      let pos = B.Position (digitToInt row, B.letterToCol col)
          dir' =
            case dir of
              '+' -> Right B.Up
              '-' -> Right B.Down
              '>' -> Right B.Right
              '<' -> Right B.Left
              _ -> Left PTNDirectionError
       in case dir' of
            Right d -> Right $ B.Slide (pos, 1, d, [1], color, False)
            Left err -> Left err
    _ -> Left PTNSlideError

extractField :: String -> [Text] -> Either PTNParseError (Maybe String)
extractField fieldName ls =
  case find (T.isPrefixOf (T.pack ("[" ++ fieldName ++ ": "))) ls of
    Just line ->
      Right $ Just $ T.unpack $ T.drop (length fieldName + 3) (T.init line)
    Nothing -> Right Nothing
