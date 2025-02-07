{-# LANGUAGE DeriveGeneric #-}
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
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleHeaders)
import qualified Network.WebSockets as WS
import qualified PTN as P
import qualified TPS
import Web.Scotty
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
    }

startServer :: IO ()
startServer = do
  gameStore <- initGameStore
  clientStore <- initClientStore
  putStrLn "Game store initialized"
  forkIO $ WS.runServer "127.0.0.1" 9160 $ websocketApp gameStore clientStore
  scotty 3000 $ do
    middleware $ cors (const $ Just appCorsResourcePolicy)
    options "/api/game/new" $ text "OK"
    options "/api/game/move" $ text "OK"
    options "/api/game/:id" $ text "OK"
    post "/api/game/new" $ do
      req <- jsonData
      gameId <- liftIO $ createNewGame gameStore (boardSize req)
      json $
        GameResponse
          Success
          "New game created"
          (Just $ TPS.gameStateToTPS $ B.getInitialGameState (boardSize req))
          (Just "White")
          (Just 1)
          (Just $ B.getInitialReserves (boardSize req))
          (Just $ B.getInitialReserves (boardSize req))
          (Just B.Continue)
          (Just [])
          (Just gameId)
    post "/api/game/move" $ do
      MoveRequest gId moveStr turn <- jsonData
      result <- liftIO $ processMove gameStore gId moveStr turn
      case result of
        Left err -> do
          let errorResponse =
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
                  (Just gId)
          json errorResponse
          clients <- liftIO $ atomically $ readTVar clientStore
          liftIO $ putStrLn $ "api/game/move updating clients error"
          case Map.lookup gId clients of
            Just clients' ->
              liftIO $
              mapM_
                (\(_, ws) -> WS.sendTextData ws (encode errorResponse))
                clients'
            Nothing -> return ()
        Right gs -> do
          let successResponse = gameStateToResponse gs gId
          json successResponse
          clients <- liftIO $ atomically $ readTVar clientStore
          liftIO $ putStrLn $ "api/game/move updating clients success"
          case Map.lookup gId clients of
            Just clients' ->
              liftIO $
              mapM_
                (\(_, ws) -> WS.sendTextData ws (encode successResponse))
                clients'
            Nothing -> return ()
    get "/api/game/:id" $ do
      gId <- param "id"
      maybeGame <- liftIO $ getGame gameStore gId
      case maybeGame of
        Nothing ->
          json $
          GameResponse
            Error
            "Game not found"
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just gId)
        Just gs -> json $ gameStateToResponse gs gId

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
          let errorResponse =
                GameResponse
                  Error
                  "Game not found"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (Just gId)
          WS.sendTextData conn (encode errorResponse)
        Just gs -> do
          let successResponse = gameStateToResponse gs gId
          WS.sendTextData conn (encode successResponse)
      forever $ do
        putStrLn "Waiting for move message..."
        msg' <- WS.receiveData conn
        case decode msg' of
          Just (MoveRequest gId' moveStr turn) -> do
            result <- processMove gameStore gId' moveStr turn
            case result of
              Right gs -> do
                putStrLn "Successfully processed move"
                let response = gameStateToResponse gs gId'
                clients <- atomically $ readTVar clientStore
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
          Nothing -> putStrLn "Failed to decode move message"
    Nothing -> putStrLn "Failed to decode initial connection message"

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Just (["http://localhost:5173"], True)
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

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
