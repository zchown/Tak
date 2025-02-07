{-# Language DeriveGeneric, OverloadedStrings #-}

import qualified Board as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Moves as M
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleHeaders)
import qualified Network.WebSockets as WS
import qualified PTN as P
import qualified TPS
import Web.Scotty

data GameStatus
  = Success
  | Error
  deriving (Show, Generic)

instance FromJSON GameStatus

instance ToJSON GameStatus

data ConnectionMessage = ConnectionMessage
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
  } deriving (Show, Generic)

instance FromJSON GameResponse

instance ToJSON GameResponse

data NewGameRequest = NewGameRequest
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


