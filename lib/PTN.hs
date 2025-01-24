{-# LANGUAGE OverloadedStrings #-}

module PTN where

import Board
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
parseMove history moveText = undefined
