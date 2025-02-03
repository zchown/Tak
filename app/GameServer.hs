{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GameServer where

import qualified Board as B
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Moves as M
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleHeaders)
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
  , gameHistory :: Maybe [B.Move]
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

initGameStore :: IO GameStore
initGameStore = newTVarIO Map.empty

startServer :: IO ()
startServer = do
  gameStore <- initGameStore
  putStrLn "Game store initialized"
  scotty 3000 $ do
    middleware $ cors (const $ Just appCorsResourcePolicy)
    options "/api/game/new" $ do text "OK"
    options "/api/game/move" $ do text "OK"
    options "/api/game/:id" $ do text "OK"
    post "/api/game/new" $ do
      req <- jsonData :: ActionM NewGameRequest
      liftIO $
        putStrLn $
        "Received new game request with board size: " ++ show (boardSize req)
      gameId <- liftIO $ createNewGame gameStore (boardSize req)
      json $
        GameResponse
          { responseStatus = Success
          , message = "New game created"
          , board =
              Just $ TPS.gameStateToTPS $ getInitialGameState (boardSize req)
          , currentPlayer = Just "White"
          , moveNum = Just 1
          , whiteReserves = Just $ getInitialReserves (boardSize req)
          , blackReserves = Just $ getInitialReserves (boardSize req)
          , gameResult = Just B.Continue
          , gameHistory = Just []
          , gameID = Just gameId
          }
    post "/api/game/move" $ do
      MoveRequest gId moveStr <- jsonData
      liftIO $ putStrLn $ "Received move request :" ++ show (gId, moveStr)
      result <- liftIO $ processMove gameStore gId moveStr
      liftIO $ putStrLn $ "Move processed"
      case result of
        Left err ->
          json $
          GameResponse
            { responseStatus = Error
            , message = err
            , board = Nothing
            , currentPlayer = Nothing
            , moveNum = Nothing
            , whiteReserves = Nothing
            , blackReserves = Nothing
            , gameResult = Nothing
            , gameHistory = Nothing
            , gameID = Just gId
            }
        Right gs ->
          json $
          GameResponse
            { responseStatus = Success
            , message = "Move processed successfully"
            , board = Just $ TPS.gameStateToTPS gs
            , currentPlayer = Just $ colorToText (B.turn gs)
            , moveNum = Just $ B.moveNumber gs
            , whiteReserves = Just $ B.player1 gs
            , blackReserves = Just $ B.player2 gs
            , gameResult = Just $ B.result gs
            , gameHistory = Just $ B.gameHistory gs
            , gameID = Just gId
            }
    get "/api/game/:id" $ do
      gId <- param "id"
      maybeGame <- liftIO $ getGame gameStore gId
      case maybeGame of
        Nothing ->
          json $
          GameResponse
            { responseStatus = Error
            , message = "Game not found"
            , board = Nothing
            , currentPlayer = Nothing
            , moveNum = Nothing
            , whiteReserves = Nothing
            , blackReserves = Nothing
            , gameResult = Nothing
            , gameHistory = Nothing
            , gameID = Just gId
            }
        Just gs ->
          json $
          GameResponse
            { responseStatus = Success
            , message = "Game state retrieved"
            , board = Just $ TPS.gameStateToTPS gs
            , currentPlayer = Just $ colorToText (B.turn gs)
            , moveNum = Just $ B.moveNumber gs
            , whiteReserves = Just $ B.player1 gs
            , blackReserves = Just $ B.player2 gs
            , gameResult = Just $ B.result gs
            , gameHistory = Just $ B.gameHistory gs
            , gameID = Just gId
            }

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
        Right move ->
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
getGame store gId =
  atomically $ do
    games <- readTVar store
    return $ Map.lookup gId games

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
          "x2,12S,x,2S/12,12,2,12112,2/x,1112112C,2,1,x/x,221S,1,2221C,1/x,11S,1,2,1 1 40"
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
