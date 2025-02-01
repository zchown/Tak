module TestGeneral where

import qualified Board as B
import Control.Monad (forM_)
import qualified Data.Text.IO as TIO
import qualified Moves as M
import qualified PTN as P
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified TPS as T

validateMoves :: B.Board -> [B.Move] -> IO ()
validateMoves _ [] = return ()
validateMoves board (move:rest) = do
  putStrLn $ "Board state: " ++ show (T.boardToTPS board)
  putStrLn $ "Validating move: " ++ show move ++ "\n"
  case M.makeMove board move of
    Left err -> error $ "Failed to make move: " ++ show err
    Right newBoard -> do
      case M.undoMove newBoard move of
        Left err -> error $ "Failed to undo move: " ++ show err
        Right undoneBoard ->
          if equalBoards board undoneBoard
            then validateMoves newBoard rest
            else error "Board state does not match after undoing move"

equalBoards :: B.Board -> B.Board -> Bool
equalBoards b1 b2 = T.boardToTPS b1 == T.boardToTPS b2

getPTNFiles :: FilePath -> IO [FilePath]
getPTNFiles dir = do
  files <- listDirectory dir
  let fullPaths = map (dir </>) files
  putStrLn $ "Found PTN files: " ++ show fullPaths
  return fullPaths

reanPTNFile :: FilePath -> IO [B.Move]
reanPTNFile file = do
  contents <- TIO.readFile file
  case P.parsePTN contents of
    Left err -> error $ "Failed to parse PTN file: " ++ show err
    Right ptn -> return $ P.moves ptn

runGeneralTests :: IO ()
runGeneralTests = do
  files <- getPTNFiles "test/ptn"
  let emptyBoard = B.createEmptyBoard 6
  forM_ files $ \file -> do
    moves <- reanPTNFile file
    validateMoves emptyBoard moves
    putStrLn $ "Passed PTN file: " ++ file
