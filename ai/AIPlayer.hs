{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module AIPlayer where

import qualified Board as B
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MoveGen as MG
import qualified Network.WebSockets as WS
import qualified TPS
import WebTypes

whiteStrategy :: B.GameState -> IO Text
whiteStrategy = MG.generateRandomMove

blackStrategy :: B.GameState -> IO Text
blackStrategy = MG.generateRandomMove

myGameId :: Text
myGameId = "game6"

gameResponseToGameState :: GameResponse -> Maybe B.GameState
gameResponseToGameState gr = do
  boardText <- board gr
  case TPS.parseTPS boardText of
    Left _ -> Nothing
    Right gs -> Just gs

startAIPlayer :: IO ()
startAIPlayer = WS.runClient "127.0.0.1" 9160 "/" clientApp

clientApp :: WS.Connection -> IO ()
clientApp conn = do
  putStrLn "Connected to game server via websockets."
  let connMsg = ConnectionMessage myGameId
  WS.sendTextData conn (encode connMsg)
  putStrLn $ "Sent connection message for game: " ++ T.unpack myGameId
  forever $ do
    putStrLn "Waiting for server message..."
    msg <- WS.receiveData conn
    case decode msg :: Maybe GameResponse of
      Just gr -> do
        case gameResponseToGameState gr of
          Just gs -> do
            let currentTurn = B.turn gs
            putStrLn $ "Current turn: " ++ show currentTurn
            case B.result gs of
              B.Continue ->
                if isOurTurn gs
                  then do
                    let strategy =
                          case currentTurn of
                            B.White -> whiteStrategy
                            B.Black -> blackStrategy
                    move <- strategy gs
                    let moveReq =
                          MoveRequest
                            myGameId
                            move
                            (T.pack $ B.colorString currentTurn)
                    WS.sendTextData conn (encode moveReq)
                    putStrLn $ "Submitted move: " ++ T.unpack move
                  else putStrLn "Not our turn, waiting..."
              _ -> putStrLn "Game is over, stopping AI."
          Nothing -> putStrLn "Failed to parse game state from response."
      Nothing -> putStrLn "Failed to decode server message."

isOurTurn :: B.GameState -> Bool
isOurTurn _ = True
