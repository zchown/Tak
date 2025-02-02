{-# LANGUAGE OverloadedStrings #-}

module TerminalGamePlayer where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

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
