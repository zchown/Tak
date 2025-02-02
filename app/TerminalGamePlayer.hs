{-# LANGUAGE OverloadedStrings #-}

module TerminalGamePlayer where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

import qualified Board as B
import qualified Moves as M
import qualified PTN as P
import qualified TPS as T

makeUserMove :: B.GameState -> IO B.GameState
makeUserMove gs@(B.GameState b c mn p1 p2 r ms) = do
  putStr $ B.colorString c ++ "Enter your move: "
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
