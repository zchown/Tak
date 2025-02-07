{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module GameServer where

import qualified Board as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Moves as M
import qualified Network.WebSockets as WS
import qualified PTN as P
import qualified TPS
import WebTypes

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
  gameStore <- initGameStore
  clientStore <- initClientStore
  putStrLn "Game store initialized"
  _ <-
    forkIO $ WS.runServer "127.0.0.1" 9160 $ websocketApp gameStore clientStore
  putStrLn "WebSocket server started"
  forever $ return ()

websocketApp :: GameStore -> ClientStore -> WS.ServerApp
websocketApp gameStore clientStore pending = do
  putStrLn "New WebSocket connection pending"
  conn <- WS.acceptRequest pending
  putStrLn "WebSocket connection accepted"
  msg <- WS.receiveData conn
  putStrLn $ "Received initial message: " ++ show msg
  case decode msg of
    Just (ConnectionMessage gId) -> do
      putStrLn $ "Successfully connected client to game: " ++ show gId
      atomically $
        modifyTVar clientStore $ Map.insertWith (++) gId [(gId, conn)]
      maybeGame <- getGame gameStore gId
      case maybeGame of
        Nothing -> do
          _ <- createNewGame gameStore 6
          let newGameResponse =
                GameResponse
                  Success
                  "New game created"
                  (Just $ TPS.gameStateToTPS $ B.getInitialGameState 6)
                  (Just "White")
                  (Just 1)
                  (Just $ B.getInitialReserves 6)
                  (Just $ B.getInitialReserves 6)
                  (Just B.Continue)
                  (Just [])
                  (Just gId)
                  (Just (0, 0))
                  (Just (0, 0))
                  (Just 0)
                  (Just False)
          WS.sendTextData conn (encode newGameResponse)
        Just gs -> do
          let successResponse = gameStateToResponse gs gId
          WS.sendTextData conn (encode successResponse)
      forever $ do
        putStrLn "Waiting for move message..."
        msg' <- WS.receiveData conn
        case decode msg' of
          Just (ResetRequest gid') -> do
            maybeGame <- getGame gameStore gid'
            case maybeGame of
              Nothing -> do
                putStrLn "Game not found"
                return ()
              Just _ -> do
                atomically $ modifyTVar gameStore $ Map.insert gid' (B.getInitialGameState 6)
                putStrLn "Game reset"
          Just (MoveRequest gId' moveStr turn) -> do
            result <- processMove gameStore gId' moveStr turn
            case result of
              Right gs -> do
                putStrLn "Successfully processed move"
                let response = gameStateToResponse gs gId'
                clients <- readTVarIO clientStore
                putStrLn "Updating clients"
                case Map.lookup gId' clients of
                  Just clients' -> do
                    mapM_
                      (\(_, ws) -> WS.sendTextData ws (encode response))
                      clients'
                    putStrLn "Clients updated"
                  Nothing -> do
                    putStrLn "No clients to update"
                    return ()
              Left err ->
                WS.sendTextData conn $
                encode $
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
                  Nothing
          Nothing -> putStrLn "Failed to decode move message"
    Nothing -> putStrLn "Failed to decode initial connection message"

createNewGame :: GameStore -> Int -> IO Text
createNewGame store size = do
  let gid = "game" <> T.pack (show size)
  let initialState = B.getInitialGameState size
  atomically $ modifyTVar store $ Map.insert gid initialState
  return gid

processMove :: GameStore -> Text -> Text -> Text -> IO (Either Text B.GameState)
processMove store gId moveStr turn = do
  maybeGame <- getGame store gId
  case maybeGame of
    Nothing -> return $ Left "Game not found"
    Just gs -> do
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
            then do
              putStrLn $
                "Not your turn: " ++
                show (B.turn gs) ++ " vs " ++ show (B.getMoveColor move')
              return $ Left "Not your turn"
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
                                    , B.result =
                                        B.checkGameResult $
                                        B.GameState
                                          newBoard
                                          (B.turn gs)
                                          1
                                          p1'
                                          p2'
                                          B.Continue
                                          []
                                    , B.gameHistory = move : B.gameHistory gs
                                    }
                            atomically $
                              modifyTVar store $ Map.insert gId newState
                            return $ Right newState
