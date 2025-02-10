{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module GameServer where

import qualified Board as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Moves as M
import qualified Network.WebSockets as WS
import qualified PTN as P
import System.Random (randomIO)
import WebTypes

startServer :: IO ()
startServer = do
  putStrLn "Initializing game store..."
  gameStore <- initGameStore
  clientStore <- initClientStore
  putStrLn "Starting WebSocket server on ws://127.0.0.1:9160..."
  WS.runServer "127.0.0.1" 9160 $ websocketApp gameStore clientStore

websocketApp :: GameStore -> ClientStore -> WS.ServerApp
websocketApp gameStore clientStore pending = do
  conn <- WS.acceptRequest pending
  clientId <- randomIO :: IO ClientID
  msg <- WS.receiveData conn
  case decode msg of
    Just (ConnectionMessage gId) -> do
      atomically $
        modifyTVar clientStore $ Map.insertWith (++) gId [(clientId, conn)]
      maybeGame <- getGame gameStore gId
      case maybeGame of
        Just gs -> WS.sendTextData conn (encode $ gameInfoToResponse gs gId)
        Nothing -> do
          newGameId <- createNewGame gameStore 6
          let gsi = B.getInitialGameState 6
          let score = getInitialGameScore
          WS.sendTextData
            conn
            (encode $ gameInfoToResponse (GameInfo gsi score) newGameId)
      finally
        (handleClient gameStore clientStore gId clientId conn)
        (removeClient clientStore gId clientId)
    Nothing -> putStrLn "Failed to decode initial connection message"

removeClient :: ClientStore -> Text -> ClientID -> IO ()
removeClient clientStore gId clientId =
  atomically $
  modifyTVar clientStore $ Map.update (removeClientById clientId) gId

removeClientById ::
     ClientID
  -> [(ClientID, WS.Connection)]
  -> Maybe [(ClientID, WS.Connection)]
removeClientById cId clients =
  let filtered = filter ((/= cId) . fst) clients
   in if null filtered
        then Nothing
        else Just filtered

handleClient ::
     GameStore -> ClientStore -> Text -> ClientID -> WS.Connection -> IO ()
handleClient gameStore clientStore gId clientId conn =
  forever $ do
    msg <- WS.receiveData conn
    case decode msg of
      Just (MoveRequest gId' moveStr turn) -> do
        result <- processMove gameStore gId' moveStr turn
        case result of
          Right gs ->
            notifyClients clientStore gId' (encode $ gameInfoToResponse gs gId')
          Left err ->
            WS.sendTextData
              conn
              (encode $
               GameResponse
                 Error
                 err
                 Nothing
                 Nothing
                 Nothing
                 Nothing
                 Nothing
                 Nothing
                 Nothing
                 (Just gId')
                 Nothing
                 Nothing
                 Nothing
                 Nothing)
      Nothing -> putStrLn "Failed to decode move message"

notifyClients :: ClientStore -> Text -> Text -> IO ()
notifyClients clientStore gId message = do
  clients <- readTVarIO clientStore
  case Map.lookup gId clients of
    Just clients' -> mapM_ (\(_, ws) -> WS.sendTextData ws message) clients'
    Nothing -> return ()

createNewGame :: GameStore -> Int -> IO Text
createNewGame store size = do
  let gid = "game" <> T.pack (show size)
  let initialState = B.getInitialGameState size
  let initialScore = getInitialGameScore
  atomically $
    modifyTVar store $ Map.insert gid (GameInfo initialState initialScore)
  return gid

processMove :: GameStore -> Text -> Text -> Text -> IO (Either Text B.GameState)
processMove store gId moveStr turn = do
  maybeGame <- getGame store gId
  case maybeGame of
    Nothing -> return $ Left "Game not found"
    Just gsi -> do
      let gs = giGameState gsi
      let score = giGameScore gsi
      case P.parseSingleMove
             (T.unpack $ T.strip moveStr)
             (B.stringColor $ T.unpack turn) of
        Left err -> return $ Left $ T.pack $ show err
        Right move' -> validateMove store gId move' moveStr turn gs score

validateMove ::
     GameStore
  -> Text
  -> B.Move
  -> Text
  -> Text
  -> B.GameState
  -> GameScore
  -> IO (Either Text B.GameState)
validateMove store gId move' moveStr turn gs score = do
  let move =
        if length (B.gameHistory gs) >= 2
          then move'
          else B.flipMoveColor move'
  if B.turn gs /= B.getMoveColor move'
    then return $ Left "Not your turn"
    else if not (B.hasReserves (B.player1 gs) (B.player2 gs) move)
           then return $ Left "Not enough pieces"
           else case M.makeMove (B.board gs) move of
                  Left (M.InvalidMove "Crush not set correctly") ->
                    processMove store gId (T.concat [moveStr, "*"]) turn
                  Left err -> return $ Left $ T.pack $ show err
                  Right newBoard ->
                    handleGameResult store gId gs move newBoard score

handleGameResult ::
     GameStore
  -> Text
  -> B.GameState
  -> B.Move
  -> B.Board
  -> GameScore
  -> IO (Either Text B.GameState)
handleGameResult store gId gs move newBoard score = do
  let gr =
        B.checkGameResult
          (B.GameState
             newBoard
             (B.turn gs)
             (B.moveNumber gs + 1)
             (B.player1 gs)
             (B.player2 gs)
             B.Continue
             (move : B.gameHistory gs))
  case gr of
    B.Continue ->
      updateGameStore store gId (B.updateGameState newBoard gs move gr) score
    B.Road c -> resetGame store gId (incrementRoadScore score c)
    B.FlatWin c -> resetGame store gId (incrementFlatScore score c)
    B.Draw -> resetGame store gId (incrementDraws score)

updateGameStore ::
     GameStore
  -> Text
  -> B.GameState
  -> GameScore
  -> IO (Either Text B.GameState)
updateGameStore store gId newState score = do
  atomically $ modifyTVar store $ Map.insert gId (GameInfo newState score)
  return $ Right newState

resetGame :: GameStore -> Text -> GameScore -> IO (Either Text B.GameState)
resetGame store gId newScore = do
  let newState = B.getInitialGameState 6
  atomically $ modifyTVar store $ Map.insert gId (GameInfo newState newScore)
  return $ Right newState
