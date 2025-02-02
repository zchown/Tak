{-# LANGUAGE OverloadedStrings #-}

module TerminalGamePlayer where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

import qualified Board as B
import qualified Moves as M
import qualified PTN as P
import qualified TPS

startGame :: IO ()
startGame = do
  envVars <- loadEnvFile
  setEnvVars envVars
  gameConfig <- getGameConfig
  let gs = getInitialGameState gameConfig
  gameLoop gameConfig gs

gameLoop :: GameConfig -> B.GameState -> IO ()
gameLoop gc gs = do
  displayGameState (display gc) gs
  result <- checkForWin gs
  case result of
    B.Continue -> do
      gs' <- makeUserMove gs
      gameLoop gc gs'
    _ -> return ()

makeUserMove :: B.GameState -> IO B.GameState
makeUserMove gs@(B.GameState b c mn p1 p2 r ms) = do
  putStrLn $ B.colorString c ++ " Enter your move: "
  t <- getLine
  case P.parseSingleMove t c of
    Left e -> do
      print e
      makeUserMove gs
    Right m -> do
      if not (B.hasReserves p1 p2 m)
        then do
          putStrLn "You don't have enough pieces to make that move."
          makeUserMove gs
        else do
          case M.makeMove b m of
            Left e -> do
              print e
              makeUserMove gs
            Right b' -> do
              let (p1', p2') = B.getNewReserves p1 p2 m
              return $
                B.GameState b' (B.flipColor c) (mn + 1) p1' p2' r (m : ms)

displayGameState :: DisplayType -> B.GameState -> IO ()
displayGameState d gs@(B.GameState b c mn p1 p2 _ _) = do
  putStrLn $ "Move number: " ++ show mn
  putStrLn $ "Reserves: " ++ show p1 ++ " " ++ show p2
  putStrLn $ "Player to move: " ++ B.colorString c
  putStrLn "Board:"
  case d of
    TPS -> putStrLn $ T.unpack $ TPS.gameStateToTPS gs
    BPO -> putStrLn $ B.boardString b

checkForWin :: B.GameState -> IO B.Result
checkForWin gs = do
  case B.checkGameResult gs of
    B.Continue -> return B.Continue
    B.Road c -> do
      putStrLn $ B.colorString c ++ " has made a road!"
      return $ B.Road c
    B.FlatWin c -> do
      putStrLn $ B.colorString c ++ " has won by flat count!"
      return $ B.FlatWin c
    B.Draw -> do
      putStrLn "The game is a draw!"
      return B.Draw

----------------------------
-- | Initial Game State | --
----------------------------
reserves4 :: B.Reserves
reserves4 = B.Reserves 15 0

reserves5 :: B.Reserves
reserves5 = B.Reserves 21 1

reserves6 :: B.Reserves
reserves6 = B.Reserves 30 1

reserves7 :: B.Reserves
reserves7 = B.Reserves 40 2

reserves8 :: B.Reserves
reserves8 = B.Reserves 50 2

getInitialGameState :: GameConfig -> B.GameState
getInitialGameState gc =
  B.GameState
    { B.board = B.createEmptyBoard (boardSize gc)
    , B.turn = B.White
    , B.moveNumber = 1
    , B.player1 = getInitialReserves (boardSize gc)
    , B.player2 = getInitialReserves (boardSize gc)
    , B.result = B.Continue
    , B.gameHistory = []
    }

getInitialReserves :: Int -> B.Reserves
getInitialReserves n
  | n == 4 = reserves4
  | n == 5 = reserves5
  | n == 6 = reserves6
  | n == 7 = reserves7
  | n == 8 = reserves8
  | otherwise = B.Reserves 0 0

----------------------
-- | Game Options | --
----------------------
data GameType
  = HH
  | HC
  | CC

data DisplayType
  = TPS
  | BPO

data GameConfig = GameConfig
  { gt :: GameType
  , display :: DisplayType
  , boardSize :: Int
  }

loadEnvFile :: IO [(String, String)]
loadEnvFile = do
  exists <- doesFileExist "gameOptions"
  if exists
    then do
      content <- TIO.readFile "gameOptions"
      return $ parseEnvFile $ T.unpack content
    else return []
  where
    parseEnvFile :: String -> [(String, String)]
    parseEnvFile = map parseLine . filter (not . null) . lines
    parseLine line =
      case break (== '=') line of
        (key, '=':value) ->
          ( (T.unpack . T.strip . T.pack) key
          , (T.unpack . T.strip . T.pack) value)
        _ -> ("", "")

setEnvVars :: [(String, String)] -> IO ()
setEnvVars = mapM_ (uncurry setEnv)

getGameConfig :: IO GameConfig
getGameConfig = do
  gType <- lookupEnv "GAME_TYPE"
  disp <- lookupEnv "DISPLAY_TYPE"
  bSize <- lookupEnv "BOARD_SIZE"
  return $
    GameConfig
      { gt =
          case gType of
            Just "HH" -> HH
            Just "HC" -> HC
            Just "CC" -> CC
            _ -> HH
      , display =
          case disp of
            Just "TPS" -> TPS
            Just "BPO" -> BPO
            _ -> BPO
      , boardSize =
          case bSize of
            Just x ->
              if (read x :: Int) > 8
                then 5
                else read x
            _ -> 5
      }
