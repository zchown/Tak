module TestGeneralMutable where

import qualified Board as B
import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as TIO
import qualified Moves as M
import qualified MutableState as MS
import qualified PTN as P
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified TPS as T

validateMoves :: MS.MGameState s -> [B.Move] -> IO ()
validateMoves _ [] = return ()
validateMoves gameState (move:rest) = do
  board <- MS.toBoard (MS.mBoard gameState)
  putStrLn $ "Board state: " ++ show (T.boardToTPS board)
  putStrLn $ "Validating move: " ++ show move ++ "\n"
  moveResult <- MS.makeMoveNoCheck (MS.mBoard gameState) move
  modifyIORef' (MS.mMoveNumber gameState) (+ 1)
  turn <- readIORef (MS.mTurn gameState)
  writeIORef (MS.mTurn gameState) (B.flipColor turn)
  boardAfterMove <- MS.toBoard (MS.mBoard gameState)
  MS.undoMoveNoChecks (MS.mBoard gameState) move
  boardAfterUndo <- MS.toBoard (MS.mBoard gameState)
  if equalBoards board boardAfterUndo
    then do
      MS.makeMoveNoCheck (MS.mBoard gameState) move
      validateMoves gameState rest
    else do
      putStrLn $ "Board after move: " ++ show (T.boardToTPS boardAfterMove)
      putStrLn $ "Board after undo: " ++ show (T.boardToTPS boardAfterUndo)
      error "Board state does not match after undoing move"

equalBoards :: B.Board -> B.Board -> Bool
equalBoards b1 b2 = T.boardToTPS b1 == T.boardToTPS b2

getPTNFiles :: FilePath -> IO [FilePath]
getPTNFiles dir = do
  files <- listDirectory dir
  let fullPaths = map (dir </>) files
  putStrLn $ "Found PTN files: " ++ show fullPaths
  return fullPaths

readPTNFile :: FilePath -> IO [B.Move]
readPTNFile file = do
  contents <- TIO.readFile file
  case P.parsePTN contents of
    Left err -> error $ "Failed to parse PTN file: " ++ show err
    Right ptn -> return $ P.moves ptn

runGeneralTestsMutable :: IO ()
runGeneralTestsMutable = do
  files <- getPTNFiles "test/ptn"
  forM_ files $ \file -> do
    emptyBoard <- MS.createMutableBoard (B.createEmptyBoard 6)
    turn <- newIORef B.White
    moveNumber <- newIORef 0
    player1 <- newIORef (B.Reserves 10 1)
    player2 <- newIORef (B.Reserves 10 1)
    resultRef <- newIORef B.Continue
    gameHistory <- newIORef []
    let gameState =
          MS.MGameState
            emptyBoard
            turn
            moveNumber
            player1
            player2
            resultRef
            gameHistory
    moves <- readPTNFile file
    putStrLn $ "Validating moves from PTN file: " ++ file
    validateMoves gameState moves
    putStrLn $ "Passed PTN file: " ++ file
