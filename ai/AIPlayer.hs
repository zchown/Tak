{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module AIPlayer where

import qualified Board as B
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified MoveGen as MG
import Network.HTTP.Simple

import qualified TPS

whiteStrategy = MG.alphaBetaBest

blackStrategy = MG.betterEval90

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
  , moveColor :: Text
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
  forever $ do
    maybeGameState <- fetchGameState myGameId
    case maybeGameState of
      Just gs -> do
        putStrLn "Fetched game state successfully."
        submitMove gs myGameId
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

submitMove :: GameResponse -> Text -> IO ()
submitMove gr gId = do
  let maybeGameState = gameResponseToGameState gr
  case maybeGameState of
    Just gs -> do
      move <-
        case B.turn gs of
          B.White -> whiteStrategy gs
          B.Black -> blackStrategy gs
      let reqBody =
            encode $ MoveRequest gId move $ T.pack $ B.colorString (B.turn gs)
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
