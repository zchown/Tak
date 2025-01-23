{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

loadEnvFile :: IO [(String, String)]
loadEnvFile = do
  exists <- doesFileExist ".tenv"
  if exists
    then do
      content <- TIO.readFile ".tenv"
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
setEnvVars = mapM_ (\(key, value) -> setEnv key value)

data TestConfig = TestConfig
  { runBoard :: Bool
  , runTPS :: Bool
  }

getTestConfig :: IO TestConfig
getTestConfig = do
  rBoard <- lookupEnvBool "RUN_BOARD" True
  rTPS <- lookupEnvBool "RUN_TPS" True
  return $
    TestConfig
      { runBoard = rBoard
      , runTPS = rTPS
      }
  where
    lookupEnvBool :: String -> Bool -> IO Bool
    lookupEnvBool name defaultValue = do
      mVal <- lookupEnv name
      return $
        case mVal of
          Nothing -> defaultValue
          Just val -> T.toLower (T.pack val) `elem` ["true", "1", "yes"]

runAllTests :: TestConfig -> IO ()
runAllTests config = do
  when (runBoard config) $
    putStrLn "Running board tests ..." >> runBoardTests
  when (runTPS config) $
    putStrLn "Running TPS tests..." >> runTPSTests

main :: IO ()
main = do
  putStrLn "Loading environment variables..."
  envVars <- loadEnvFile
  setEnvVars envVars
  putStrLn "Configuring tests..."
  config <- getTestConfig
  putStrLn "Running enabled tests..."
  runAllTests config
