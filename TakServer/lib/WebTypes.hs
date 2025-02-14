{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module WebTypes where

import qualified Board as B
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import qualified PTN as P
import qualified TPS

data GameStatus
  = Success
  | Error
  deriving (Show, Generic)

data GameScore = GameScore
  { p1 :: (Int, Int)
  , p2 :: (Int, Int)
  , gsDraws :: Int
  , gsSwap :: Bool
  } deriving (Show, Generic)

getInitialGameScore :: GameScore
getInitialGameScore = GameScore (0, 0) (0, 0) 0 False

incrementRoadScore :: GameScore -> B.Color -> GameScore
incrementRoadScore gs B.White =
  if gsSwap gs
    then gs {p2 = (fst (p2 gs) + 1, snd (p2 gs)), gsSwap = not (gsSwap gs)}
    else gs {p1 = (fst (p1 gs) + 1, snd (p1 gs)), gsSwap = not (gsSwap gs)}
incrementRoadScore gs B.Black =
  if gsSwap gs
    then gs {p1 = (fst (p1 gs) + 1, snd (p1 gs)), gsSwap = not (gsSwap gs)}
    else gs {p2 = (fst (p2 gs) + 1, snd (p2 gs)), gsSwap = not (gsSwap gs)}

incrementDraws :: GameScore -> GameScore
incrementDraws gs = gs {gsDraws = gsDraws gs + 1, gsSwap = not (gsSwap gs)}

incrementFlatScore :: GameScore -> B.Color -> GameScore
incrementFlatScore gs B.White =
  if gsSwap gs
    then gs {p2 = (fst (p2 gs), snd (p2 gs) + 1)}
    else gs {p1 = (fst (p1 gs), snd (p1 gs) + 1)}
incrementFlatScore gs B.Black =
  if gsSwap gs
    then gs {p1 = (fst (p1 gs), snd (p1 gs) + 1)}
    else gs {p2 = (fst (p2 gs), snd (p2 gs) + 1)}

data GameInfo = GameInfo
  { giGameState :: B.GameState
  , giGameScore :: GameScore
  } deriving (Show, Generic)

instance FromJSON GameStatus

instance ToJSON GameStatus

newtype ConnectionMessage = ConnectionMessage
  { gameId :: Text
  } deriving (Show, Generic)

instance FromJSON ConnectionMessage

instance ToJSON ConnectionMessage

data MoveRequest = MoveRequest
  { moveGameId :: Text
  , moveNotation :: Text
  , moveColor :: Text
  } deriving (Show, Generic)

instance FromJSON MoveRequest

instance ToJSON MoveRequest

data GameResponse = GameResponse
  { responseStatus :: GameStatus
  , message :: Text
  , board :: Maybe Text
  , currentPlayer :: Maybe Text
  , moveNum :: Maybe Int
  , whiteReserves :: Maybe B.Reserves
  , blackReserves :: Maybe B.Reserves
  , gameResult :: Maybe B.Result
  , gameHistory :: Maybe [Text]
  , gameID :: Maybe Text
  , player1 :: Maybe (Int, Int)
  , player2 :: Maybe (Int, Int)
  , draws :: Maybe Int
  , swap :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON GameResponse

instance ToJSON GameResponse

newtype NewGameRequest = NewGameRequest
  { boardSize :: Int
  } deriving (Show, Generic)

instance FromJSON NewGameRequest

instance ToJSON NewGameRequest

type GameStore = TVar (Map.Map Text GameInfo)

type Client = (Int, WS.Connection)

type ClientID = Int

type ClientStore = TVar (Map.Map Text [Client])

initGameStore :: IO GameStore
initGameStore = newTVarIO Map.empty

initClientStore :: IO ClientStore
initClientStore = newTVarIO Map.empty

getGame :: GameStore -> Text -> IO (Maybe GameInfo)
getGame store gId = atomically $ Map.lookup gId <$> readTVar store

gameInfoToResponse :: GameInfo -> Text -> GameResponse
gameInfoToResponse (GameInfo gs score) gId =
  GameResponse
    { responseStatus = Success
    , message = "Game state retrieved"
    , board = Just $ TPS.gameStateToTPS gs
    , currentPlayer = Just $ T.pack $ B.colorString (B.turn gs)
    , moveNum = Just $ B.moveNumber gs
    , whiteReserves = Just $ B.player1 gs
    , blackReserves = Just $ B.player2 gs
    , gameResult = Just $ B.result gs
    , gameHistory = Just $ map P.moveToText (B.gameHistory gs)
    , gameID = Just gId
    , player1 = Just $ p1 score
    , player2 = Just $ p2 score
    , draws = Just $ gsDraws score
    , swap = Just $ gsSwap score
    }

errorResponse :: Text -> Text -> GameResponse
errorResponse msg gId =
  GameResponse
    { responseStatus = Error
    , message = msg
    , board = Nothing
    , currentPlayer = Nothing
    , moveNum = Nothing
    , whiteReserves = Nothing
    , blackReserves = Nothing
    , gameResult = Nothing
    , gameHistory = Nothing
    , gameID = Just gId
    , player1 = Nothing
    , player2 = Nothing
    , draws = Nothing
    , swap = Nothing
    }
