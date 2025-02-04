{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module AIPlayer where

import qualified Board as B
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Moves as M
import Network.HTTP.Simple
import qualified PTN
import System.Random (randomRIO)
import qualified TPS

-- API Configuration
apiBaseUrl :: String
apiBaseUrl = "http://localhost:3000/api/game"

myGameId :: Text
myGameId = "game6"

data GameStatus
  = Success
  | Error
  deriving (Show, Generic)

instance FromJSON GameStatus

instance ToJSON GameStatus

data MoveRequest = MoveRequest
  { moveGameId :: Text
  , moveNotation :: Text
  } deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

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

startAIPlayer :: IO ()
startAIPlayer = do
  putStrLn "AI Player started..."
  forever $
    -- threadDelay 1000
   do
    maybeGameState <- fetchGameState myGameId
    case maybeGameState of
      Just gs -> do
        putStrLn "Fetched game state successfully."
        submitRandomMove gs myGameId
      Nothing -> putStrLn "Failed to fetch game state."

fetchGameState :: Text -> IO (Maybe GameResponse)
fetchGameState gId = do
  let url = apiBaseUrl ++ "/" ++ T.unpack gId
  request <- parseRequest url
  -- putStrLn $ show request
  response <- httpLBS request
  let result = decode (getResponseBody response)
  case result of
    Just gs
      -- putStrLn $ "Fetched game state: " ++ show gs
     -> do
      return result
    Nothing -> do
      putStrLn "Failed to decode game state"
      return Nothing

submitRandomMove :: GameResponse -> Text -> IO ()
submitRandomMove gr gId = do
  let maybeGameState = gameResponseToGameState gr
  case maybeGameState of
    Just gs -> do
      move <- generateRandomMove gs
      let reqBody = encode $ MoveRequest gId move
      request <- parseRequest (apiBaseUrl ++ "/move")
      let request' =
            setRequestBodyLBS reqBody $
            setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] request
      response <- httpLBS request'
      putStrLn $ "Submitted move: " ++ T.unpack move
      -- putStrLn $ "Response: " ++ BL.unpack (getResponseBody response)
    Nothing -> putStrLn "Failed to convert game response to game state."

gameResponseToGameState :: GameResponse -> Maybe B.GameState
gameResponseToGameState gr = do
  boardText <- board gr
  case TPS.parseTPS boardText of
    Left _ -> Nothing
    Right gs -> Just gs

generateRandomMove :: B.GameState -> IO Text
generateRandomMove gs = do
  let moves = M.generateAllMoves gs
  if null moves
    then return "No valid moves"
    else do
      randomIndex <- randomRIO (0, length moves - 1)
      let move = moves V.! randomIndex
      return $ PTN.moveToText move
