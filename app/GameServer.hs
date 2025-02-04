{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GameServer where

import qualified Board as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
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

data MoveRequest = MoveRequest
  { gameId :: Text
  , moveNotation :: Text
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
          (Just $ TPS.gameStateToTPS $ getInitialGameState (boardSize req))
          (Just "White")
          (Just 1)
          (Just $ getInitialReserves (boardSize req))
          (Just $ getInitialReserves (boardSize req))
          (Just B.Continue)
          (Just [])
          (Just gameId)
    post "/api/game/move" $ do
      MoveRequest gId moveStr <- jsonData
      result <- liftIO $ processMove gameStore gId moveStr
      case result of
        Left err ->
          json $
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
        Right gs -> do
          clients <- liftIO $ atomically $ readTVar clientStore
          case Map.lookup gId clients of
            Just clients' ->
              liftIO $
              mapM_ (\(_, ws) -> WS.sendTextData ws (encode gs)) clients'
            Nothing -> return ()
          json $
            GameResponse
              Success
              "Move processed successfully"
              (Just $ TPS.gameStateToTPS gs)
              (Just $ colorToText (B.turn gs))
              (Just $ B.moveNumber gs)
              (Just $ B.player1 gs)
              (Just $ B.player2 gs)
              (Just $ B.result gs)
              (Just $ map P.moveToText (B.gameHistory gs))
              (Just gId)
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
        Just gs ->
          json $
          GameResponse
            Success
            "Game state retrieved"
            (Just $ TPS.gameStateToTPS gs)
            (Just $ colorToText (B.turn gs))
            (Just $ B.moveNumber gs)
            (Just $ B.player1 gs)
            (Just $ B.player2 gs)
            (Just $ B.result gs)
            (Just $ map P.moveToText (B.gameHistory gs))
            (Just gId)

websocketApp :: GameStore -> ClientStore -> WS.ServerApp
websocketApp gameStore clientStore pending = do
  conn <- WS.acceptRequest pending
  msg <- WS.receiveData conn
  case decode msg of
    Just (MoveRequest gId _) -> do
      atomically $
        modifyTVar clientStore $ Map.insertWith (++) gId [(gId, conn)]
      forever $ do
        msg' <- WS.receiveData conn
        case decode msg' of
          Just (MoveRequest gId' moveStr) -> do
            result <- processMove gameStore gId' moveStr
            case result of
              Right gs -> do
                clients <- atomically $ readTVar clientStore
                case Map.lookup gId' clients of
                  Just clients' ->
                    mapM_ (\(_, ws) -> WS.sendTextData ws (encode gs)) clients'
                  Nothing -> return ()
              Left _ -> return ()
          Nothing -> return ()
    Nothing -> return ()

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  CorsResourcePolicy
    (Just (["http://localhost:5173"], True))
    ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    simpleHeaders
    Nothing
    Nothing
    False
    False
    False

createNewGame :: GameStore -> Int -> IO Text
createNewGame store size = do
  let gid = "game-" <> T.pack (show size)
  let initialState = getInitialGameState size
  atomically $ modifyTVar store $ Map.insert gid initialState
  return gid

processMove :: GameStore -> Text -> Text -> IO (Either Text B.GameState)
processMove store gId moveStr = do
  maybeGame <- getGame store gId
  case maybeGame of
    Nothing -> return $ Left "Game not found"
    Just gs -> do
      case P.parseSingleMove (T.unpack (T.strip moveStr)) (B.turn gs) of
        Left err -> return $ Left $ T.pack $ show err
        Right move' -> do
          let move =
                if length (B.gameHistory gs) >= 2
                  then move'
                  else B.flipMoveColor move'
          if not (B.hasReserves (B.player1 gs) (B.player2 gs) move)
            then return $ Left "Not enough pieces"
            else case M.makeMove (B.board gs) move of
                   Left (M.InvalidMove "Crush not set correctly") ->
                     processMove store gId (T.concat [moveStr, T.pack "*"])
                   Left err -> return $ Left $ T.pack $ show err
                   Right newBoard -> do
                     let (p1', p2') =
                           B.getNewReserves (B.player1 gs) (B.player2 gs) move
                     let newState =
                           B.GameState
                             { B.board = newBoard
                             , B.turn = B.flipColor (B.turn gs)
                             , B.moveNumber = B.moveNumber gs + 1
                             , B.player1 = p1'
                             , B.player2 = p2'
                             , B.result = B.checkGameResult gs
                             , B.gameHistory = move : B.gameHistory gs
                             }
                     atomically $ modifyTVar store $ Map.insert gId newState
                     return $ Right newState

getGame :: GameStore -> Text -> IO (Maybe B.GameState)
getGame store gId = atomically $ Map.lookup gId <$> readTVar store

colorToText :: B.Color -> Text
colorToText B.White = "White"
colorToText B.Black = "Black"

getInitialGameState :: Int -> B.GameState
getInitialGameState size =
  B.GameState
    -- { B.board = B.createEmptyBoard size
    { B.board =
        B.board $
        TPS.parseTPSHard
          "2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,2,1S/x2,2,1,21,1 1 42"
    , B.turn = B.White
    , B.moveNumber = 1
    , B.player1 = getInitialReserves size
    , B.player2 = getInitialReserves size
    , B.result = B.Continue
    , B.gameHistory = []
    }

getInitialReserves :: Int -> B.Reserves
getInitialReserves n
  | n == 4 = B.Reserves 15 0
  | n == 5 = B.Reserves 21 1
  | n == 6 = B.Reserves 30 1
  | n == 7 = B.Reserves 40 2
  | n == 8 = B.Reserves 50 2
  | otherwise = B.Reserves 0 0
