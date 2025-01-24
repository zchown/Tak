{-# LANGUAGE OverloadedStrings #-}

module PTN where

import Board
import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

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
  , moves :: History
  } deriving (Show)

parsePTN :: Text -> Either ParseError PTN
parsePTN input = parse ptnParser "PTN" input

ptnParser :: Parser PTN
ptnParser = do
  metadata <- many metadataLine
  let metadataMap = foldr (\(k, v) acc -> (k, v) : acc) [] metadata
  movesText <- many moveLine
  return $
    PTN
      { site = lookupMetadata "Site" metadataMap
      , event = lookupMetadata "Event" metadataMap
      , date = lookupMetadata "Date" metadataMap
      , time = lookupMetadata "Time" metadataMap
      , p1 = lookupMetadata "Player1" metadataMap
      , p2 = lookupMetadata "Player2" metadataMap
      , clock = lookupMetadata "Clock" metadataMap
      , ptnResult = lookupMetadata "Result" metadataMap
      , size = read $ lookupMetadata "Size" metadataMap
      , moves = parseHistoryMoves movesText
      }

lookupMetadata :: String -> [(String, String)] -> String
lookupMetadata key metadata = maybe "" id $ lookup key metadata

metadataLine :: Parser (String, String)
metadataLine = do
  char '['
  key <- many (noneOf " ")
  skipMany space
  value <- between (char '"') (char '"') (many (noneOf "\""))
  char ']'
  newline
  return (key, value)

moveLine :: Parser String
moveLine = do
  skipMany space
  line <- many (noneOf "\n")
  optional newline
  return line

parseHistoryMoves :: [String] -> History
parseHistoryMoves movesText =
  foldl' parseMove [] (filter (not . null) movesText)

parseMove :: History -> String -> History
parseMove history moveText =
  let parsedMove =
        case words moveText of
          [pos] ->
            let (col, row) = splitPos pos
                color =
                  if head pos == '1'
                    then White
                    else Black
                cleanPos =
                  if head pos `elem` ['1', '2']
                    then tail pos
                    else pos
             in PlaceFlat
                  ( Position
                      (read [last cleanPos])
                      (letterToCol (head cleanPos))
                  , color)
          ['S':pos] ->
            let (col, row) = splitPos pos
                color =
                  if head pos == '1'
                    then White
                    else Black
                cleanPos =
                  if head pos == '1'
                    then tail pos
                    else pos
             in PlaceStanding
                  ( Position
                      (read [last cleanPos])
                      (letterToCol (head cleanPos))
                  , color)
          ['C':pos] ->
            let (col, row) = splitPos pos
                color =
                  if head pos == '1'
                    then White
                    else Black
                cleanPos =
                  if head pos == '1'
                    then tail pos
                    else tail pos
             in PlaceCap
                  ( Position
                      (read [last cleanPos])
                      (letterToCol (head cleanPos))
                  , color)
          [moveNotation] ->
            let (_, start, dir, drops, crush) = parseSlideNotation moveNotation
                color =
                  if head moveNotation == '1'
                    then White
                    else Black
                (col, row) =
                  splitPos (dropWhile (`elem` ("12" :: String)) start)
                direction = parseDirection dir
             in Slide
                  ( Position (read [last start]) (letterToCol (head start))
                  , direction
                  , drops
                  , color
                  , crush)
          _ -> error $ "Can't parse move: " ++ moveText
   in parsedMove : history

parseSlideNotation :: String -> (Int, String, Char, [Int], Crush)
parseSlideNotation notation =
  let (countStr, rest) = span isDigit notation
      count =
        if null countStr
          then 1
          else read countStr
      (start, dirRest) = span isAlpha rest
      (dir, dropStr) = span (`elem` ("><+-" :: String)) dirRest
      drops =
        if null dropStr
          then [count]
          else map (\c -> read [c]) dropStr
      crush = '*' `elem` notation
   in (count, start, head dir, drops, crush)

parseDirection :: Char -> Direction
parseDirection '<' = Board.Left
parseDirection '>' = Board.Right
parseDirection '+' = Up
parseDirection '-' = Down
parseDirection c = error $ "Unknown direction: " ++ [c]

splitPos :: String -> (Char, Int)
splitPos pos =
  let cleanPos = dropWhile (`elem` ("12" :: String)) pos
   in (head cleanPos, read [last cleanPos])
