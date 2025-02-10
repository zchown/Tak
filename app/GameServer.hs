{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module GameServer where

import qualified Board as B
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
  putStrLn "Initializing client store..."
  clientStore <- initClientStore
  putStrLn "Game store initialized."
  putStrLn "Starting WebSocket server on ws://127.0.0.1:9160..."
  WS.runServer "127.0.0.1" 9160 $ \pending -> do
    putStrLn "New WebSocket connection pending"
    websocketApp gameStore clientStore pending
  putStrLn "This line should never be reached"

websocketApp :: GameStore -> ClientStore -> WS.ServerApp
websocketApp gameStore clientStore pending = do
  putStrLn "New WebSocket connection pending"
  conn <- WS.acceptRequest pending
  putStrLn "WebSocket connection accepted"
  clientId <- randomIO :: IO ClientID
  msg <- WS.receiveData conn
  putStrLn $ "Received initial message: " ++ show msg
  case decode msg of
    Just (ConnectionMessage gId) -> do
      putStrLn $ "Successfully connected client to game: " ++ show gId
      atomically $
        modifyTVar clientStore $ Map.insertWith (++) gId [(clientId, conn)]
      maybeGame <- getGame gameStore gId
      case maybeGame of
        Just gs -> do
          let response = gameInfoToResponse gs gId
          WS.sendTextData conn (encode response)
        Nothing -> do
          putStrLn "Game not found, creating new game..."
          newGameId <- createNewGame gameStore 6
          putStrLn $ "New game created with ID: " ++ show newGameId
          let gsi = B.getInitialGameState 6
          let score = getInitialGameScore
          let response = gameInfoToResponse (GameInfo gsi score) newGameId
          WS.sendTextData conn (encode response)
      finally
        (handleClient gameStore clientStore conn)
        (removeClient clientStore gId clientId)
    Nothing -> putStrLn "Failed to decode initial connection message"

removeClient :: ClientStore -> Text -> ClientID -> IO ()
removeClient clientStore gId clientId = do
  putStrLn "Removing client from game"
  atomically $
    modifyTVar clientStore $ Map.update (removeClientById clientId) gId

removeClientById ::
     ClientID
  -> [(ClientID, WS.Connection)]
  -> Maybe [(ClientID, WS.Connection)]
removeClientById cId clients =
  let filtered = filter (\(id, _) -> id /= cId) clients
   in if null filtered
        then Nothing
        else Just filtered

notifyClients :: ClientStore -> Text -> GameResponse -> IO ()
notifyClients clientStore gId message = do
  putStrLn "Notifying clients of game update"
  clients <- readTVarIO clientStore
  case Map.lookup gId clients of
    Just clients' ->
      mapM_ (\(_, ws) -> WS.sendTextData ws (encode message)) clients'
    Nothing -> return ()

handleClient :: GameStore -> ClientStore -> WS.Connection -> IO ()
handleClient gameStore clientStore conn =
  forever $ do
    msg <- WS.receiveData conn
    putStrLn $ "Received message: " ++ show msg
    case decode msg of
      Just (MoveRequest gId' moveStr turn) -> do
        result <- processMove gameStore gId' moveStr turn
        case result of
          Right gs ->
            notifyClients clientStore gId' (gameInfoToResponse gs gId')
          Left err -> WS.sendTextData conn (encode $ errorResponse err gId')
      Nothing -> putStrLn "Failed to decode move message"

createNewGame :: GameStore -> Int -> IO Text
createNewGame store size = do
  let gid = "game" <> T.pack (show size)
  let initialState = B.getInitialGameState size
  let initialScore = getInitialGameScore
  atomically $
    modifyTVar store $ Map.insert gid (GameInfo initialState initialScore)
  return gid

processMove :: GameStore -> Text -> Text -> Text -> IO (Either Text GameInfo)
processMove store gId moveStr turn = do
  maybeGame <- getGame store gId
  case maybeGame of
    Nothing -> return $ Left "Game not found"
    Just gsi -> do
      let move =
            P.parseSingleMove (T.unpack moveStr) (B.stringColor (T.unpack turn))
      case move of
        Left err -> return $ Left $ T.pack $ show err
        Right move' -> validateMove store gId move' moveStr turn gsi

validateMove ::
     GameStore
  -> Text
  -> B.Move
  -> Text
  -> Text
  -> GameInfo
  -> IO (Either Text GameInfo)
validateMove store gId move' moveStr turn gi@(GameInfo gs score)
  | B.turn gs /= B.getMoveColor move' = return $ Left "Not your turn"
  | not (B.hasReserves (B.player1 gs) (B.player2 gs) move) =
    return $ Left "Not enough pieces"
  | otherwise = do
    case M.makeMove (B.board gs) move of
      Left (M.InvalidMove "Crush not set correctly") -> do
        if B.getCrush move
          then do
            putStrLn "Crush not set correctly attempting to fix..."
            processMove store gId (T.concat [moveStr, T.pack "*"]) turn
          else return $ Left "Crush not set correctly"
      Left err -> putStrLn "Invalid move" >> return (Left $ T.pack $ show err)
      Right newBoard -> handleGameResult store gId gs move newBoard score
  where
    move =
      if length (B.gameHistory gs) >= 2 --leq bc move hasn't been added to history yet
        then move'
        else B.flipMoveColor move'

handleGameResult ::
     GameStore
  -> Text
  -> B.GameState
  -> B.Move
  -> B.Board
  -> GameScore
  -> IO (Either Text GameInfo)
handleGameResult store gId gs move newBoard score = do
  let gr =
        B.checkGameResult
          (B.GameState
             newBoard
             (B.turn gs)
             0
             (B.player1 gs)
             (B.player2 gs)
             B.Continue
             [])
  case gr of
    B.Continue ->
      updateGameStore store gId (B.updateGameState newBoard gs move gr) score
    B.Road c -> resetGame store gId (incrementRoadScore score c)
    B.FlatWin c -> resetGame store gId (incrementFlatScore score c)
    B.Draw -> resetGame store gId (incrementDraws score)

updateGameStore ::
     GameStore -> Text -> B.GameState -> GameScore -> IO (Either Text GameInfo)
updateGameStore store gId newState score = do
  putStrLn "Updating game store"
  atomically $ modifyTVar store $ Map.insert gId (GameInfo newState score)
  return $ Right (GameInfo newState score)

resetGame :: GameStore -> Text -> GameScore -> IO (Either Text GameInfo)
resetGame store gId newScore = do
  putStrLn "Resetting game"
  let newState = B.getInitialGameState 6
  atomically $ modifyTVar store $ Map.insert gId (GameInfo newState newScore)
  return $ Right (GameInfo newState newScore)
