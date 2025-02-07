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
import qualified TPS
import WebTypes

type ClientID = Int -- Unique identifier for each client

gameStateToResponse :: B.GameState -> Text -> GameResponse
gameStateToResponse gs gId =
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
    , player1 = Just (0, 0)
    , player2 = Just (0, 0)
    , draws = Just 0
    , swap = Just False
    }

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
          let response = gameStateToResponse gs gId
          WS.sendTextData conn (encode response)
        Nothing -> do
          putStrLn "Game not found, creating new game..."
          newGameId <- createNewGame gameStore 6
          putStrLn $ "New game created with ID: " ++ show newGameId
          let response = gameStateToResponse (B.getInitialGameState 6) newGameId
          WS.sendTextData conn (encode response)
      let removeClient =
            atomically $
            modifyTVar clientStore $ Map.update (removeClientById clientId) gId
      finally
        (handleClient gameStore clientStore gId clientId conn)
        removeClient
    Nothing -> putStrLn "Failed to decode initial connection message"

removeClientById ::
     ClientID
  -> [(ClientID, WS.Connection)]
  -> Maybe [(ClientID, WS.Connection)]
removeClientById cId clients =
  let filtered = filter (\(id, _) -> id /= cId) clients
   in if null filtered
        then Nothing
        else Just filtered

handleClient ::
     GameStore -> ClientStore -> Text -> ClientID -> WS.Connection -> IO ()
handleClient gameStore clientStore gId clientId conn =
  forever $ do
    msg' <- WS.receiveData conn
    putStrLn $ "Received message: " ++ show msg'
    case decode msg' of
      Just (MoveRequest gId' moveStr turn) -> do
        result <- processMove gameStore gId' moveStr turn
        case result of
          Right gs -> do
            let response = gameStateToResponse gs gId'
            clients <- readTVarIO clientStore
            case Map.lookup gId' clients of
              Just clients' ->
                mapM_
                  (\(_, ws) -> WS.sendTextData ws (encode response))
                  clients'
              Nothing -> putStrLn "No clients to update"
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
             (T.unpack (T.strip moveStr))
             (B.stringColor (T.unpack turn)) of
        Left err -> return $ Left $ T.pack $ show err
        Right move' -> do
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
                            processMove
                              store
                              gId
                              (T.concat [moveStr, T.pack "*"])
                              turn
                          Left err -> return $ Left $ T.pack $ show err
                          Right newBoard -> do
                            let gr =
                                  B.checkGameResult $
                                  B.GameState
                                    newBoard
                                    (B.turn gs)
                                    1
                                    (B.player1 gs)
                                    (B.player2 gs)
                                    B.Continue
                                    []
                            case gr of
                              B.Continue -> do
                                let (p1', p2') =
                                      B.getNewReserves
                                        (B.player1 gs)
                                        (B.player2 gs)
                                        move
                                let newState =
                                      B.GameState
                                        { B.board = newBoard
                                        , B.turn = B.flipColor (B.turn gs)
                                        , B.moveNumber = B.moveNumber gs + 1
                                        , B.player1 = p1'
                                        , B.player2 = p2'
                                        , B.result = gr
                                        , B.gameHistory =
                                            move : B.gameHistory gs
                                        }
                                atomically $
                                  modifyTVar store $
                                  Map.insert gId (GameInfo newState score)
                                return $ Right newState
                              B.Road c -> do
                                let newState = B.getInitialGameState 6
                                let newScore = incrementRoadScore score c
                                atomically $
                                  modifyTVar store $
                                  Map.insert gId (GameInfo newState newScore)
                                return $ Right newState
                              B.FlatWin c -> do
                                let newState = B.getInitialGameState 6
                                let newScore = incrementFlatScore score c
                                atomically $
                                  modifyTVar store $
                                  Map.insert gId (GameInfo newState newScore)
                                return $ Right newState
                              B.Draw -> do
                                let newState = B.getInitialGameState 6
                                let newScore = incrementDraws score
                                atomically $
                                  modifyTVar store $
                                  Map.insert gId (GameInfo newState newScore)
                                return $ Right newState
