{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module WebTypes where

import qualified Board as B
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS

data GameStatus
  = Success
  | Error
  deriving (Show, Generic)

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
  } | ResetRequest
  { resetGameId :: Text
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

type GameStore = TVar (Map.Map Text B.GameState)

type Client = (Text, WS.Connection)

type ClientStore = TVar (Map.Map Text [Client])

initGameStore :: IO GameStore
initGameStore = newTVarIO Map.empty

initClientStore :: IO ClientStore
initClientStore = newTVarIO Map.empty

getGame :: GameStore -> Text -> IO (Maybe B.GameState)
getGame store gId = atomically $ Map.lookup gId <$> readTVar store
