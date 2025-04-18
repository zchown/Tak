{-# LANGUAGE OverloadedStrings #-}

module TestMutableState where

import qualified Board as B
import Data.IORef
import qualified Data.Matrix as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import qualified Moves
import qualified MutableState as MS
import qualified TPS
import Test.Hspec

runMutableStateTests :: IO ()
runMutableStateTests =
  hspec $ do
    describe "convert back and forth with regular board" $ do
      it "should create a new board" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        putStrLn "Created mutable board"
        board' <- MS.toBoard mutBoard
        board' `shouldBe` board
      it "should be able to swap more complex board" $ do
        let board =
              B.board $
              TPS.parseTPSHard $
              T.pack "x6/x6/x6/x,212121,x4/22,12,2,2,2,12/x6 1 31"
        mutBoard <- MS.createMutableBoard board
        board' <- MS.toBoard mutBoard
        board' `shouldBe` board
    describe "correctly reads squares" $ do
      it "converts from pos to index" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        let index = MS.posToIndex mutBoard pos
        index `shouldBe` 0
        let pos' = B.Position (2, 3)
        let index' = MS.posToIndex mutBoard pos'
        index' `shouldBe` 8
        let pos'' = B.Position (4, 3)
        let index'' = MS.posToIndex mutBoard pos''
        index'' `shouldBe` 20
      it "converts from index to pos" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let index = 5
        let pos = MS.indexToPos mutBoard index
        pos `shouldBe` B.Position (1, 6)
        let index' = 35
        let pos' = MS.indexToPos mutBoard index'
        pos' `shouldBe` B.Position (6, 6)
      it "reads squares" $ do
        let board =
              B.board $
              TPS.parseTPSHard $
              T.pack "x6/x6/x2,2,2C,x2/x,212121,x,1S,x2/22,12,2,2,2,12/x6 1 31"
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        square <- MS.readSquare mutBoard pos
        square `shouldBe` []
        let pos' = B.Position (2, 2)
        square' <- MS.readSquare mutBoard pos'
        square' `shouldBe` [B.Piece B.Black B.Flat, B.Piece B.White B.Flat]
        square''' <- MS.readSquare mutBoard (B.Position (4, 3))
        square''' `shouldBe` [B.Piece B.White B.Standing]
    describe "writing squares" $ do
      it "writes to an empty square" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        MS.writeSquare mutBoard pos [B.Piece B.White B.Flat]
        square <- MS.readSquare mutBoard pos
        square `shouldBe` [B.Piece B.White B.Flat]
      it "overwrites an existing square" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        MS.writeSquare mutBoard pos [B.Piece B.White B.Flat]
        MS.writeSquare mutBoard pos [B.Piece B.Black B.Standing]
        square <- MS.readSquare mutBoard pos
        square `shouldBe` [B.Piece B.Black B.Standing]
    describe "making moves" $ do
      it "places a flat stone on an empty square" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let move = B.PlaceFlat (B.Position (1, 1), B.White)
        result <- MS.makeMove mutBoard move
        result `shouldBe` Right ()
        square <- MS.readSquare mutBoard (B.Position (1, 1))
        square `shouldBe` [B.Piece B.White B.Flat]
    it "places a flat stone on an empty square" $ do
      let board = B.createEmptyBoard 6
      mutBoard <- MS.createMutableBoard board
      let move = B.PlaceFlat (B.Position (4, 3), B.White)
      result <- MS.makeMove mutBoard move
      result `shouldBe` Right ()
      square <- MS.readSquare mutBoard (B.Position (4, 3))
      b <- MS.toBoard mutBoard
      putStrLn $ T.unpack $ TPS.boardToTPS b
      square `shouldBe` [B.Piece B.White B.Flat]
    it "places a flat stone on an empty square" $ do
      let board = B.createEmptyBoard 6
      mutBoard <- MS.createMutableBoard board
      let move = B.PlaceFlat (B.Position (1, 6), B.White)
      result <- MS.makeMove mutBoard move
      result `shouldBe` Right ()
      square <- MS.readSquare mutBoard (B.Position (1, 6))
      b <- MS.toBoard mutBoard
      putStrLn $ T.unpack $ TPS.boardToTPS b
      square `shouldBe` [B.Piece B.White B.Flat]
    describe "undoing moves" $ do
      it "undoes a flat stone placement" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let move = B.PlaceFlat (B.Position (1, 1), B.White)
        _ <- MS.makeMove mutBoard move
        MS.undoMoveNoChecks mutBoard move
        square <- MS.readSquare mutBoard (B.Position (1, 1))
        square `shouldBe` []
      it "undoes a slide move" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let move = B.PlaceFlat (B.Position (1, 1), B.White)
        _ <- MS.makeMove mutBoard move
        let slideMove =
              B.Slide (B.Position (1, 1), 1, B.Up, [1], B.White, False)
        _ <- MS.makeMove mutBoard slideMove
        MS.undoMoveNoChecks mutBoard slideMove
        square <- MS.readSquare mutBoard (B.Position (1, 1))
        square `shouldBe` [B.Piece B.White B.Flat]
    describe "move generation" $ do
      it "generates all valid first moves" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 1
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        moves <- MS.generateAllMoves gameState
        VM.length moves `shouldBe` 36
    describe "game end conditions" $ do
      it "detects a road win for White" $ do
        let board =
              B.board $
              TPS.parseTPSHard $ T.pack "x6/x6/x6/x6/x6/1,1,1,1,1,1 2 2"
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkGameWin mutBoard B.White
        result `shouldBe` B.Road B.White
      it "detects a road win for Black" $ do
        let board =
              B.board $
              TPS.parseTPSHard $ T.pack "2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2"
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.Black
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkGameWin mutBoard B.Black
        result `shouldBe` B.Road B.Black
      it "detects win for white" $ do
        let board =
              B.board $
              TPS.parseTPSHard
                "x5,2/x2,2,2,2,x/x,12,2,1,1,x/x,2,2C,1C,1,1/2,1,1,1,x2/1,1,x4 2 21"
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 21
        player1Ref <- newIORef (B.Reserves 21 0)
        player2Ref <- newIORef (B.Reserves 21 0)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkGameWin mutBoard B.White
        result `shouldBe` B.Road B.White
      it "detects a flat win for White" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        MS.writeSquare mutBoard pos [B.Piece B.White B.Flat]
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 0 0)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkFullBoard mutBoard True
        result `shouldBe` B.FlatWin B.White
      it "detects a flat win for Black" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        let pos = B.Position (1, 1)
        MS.writeSquare mutBoard pos [B.Piece B.Black B.Flat]
        turnRef <- newIORef B.Black
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 0 0)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkFullBoard mutBoard True
        result `shouldBe` B.FlatWin B.Black
      it "detects a draw due to reserves" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 0 0)
        player2Ref <- newIORef (B.Reserves 0 0)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkFullBoard mutBoard True
        result `shouldBe` B.Draw
      it "detects a road win with a capstone" $ do
        let board =
              B.board $
              TPS.parseTPSHard $
              T.pack "2,x4,1/1,1,1,1,1,x/1,1,12,2,x2/2,2,2C,2,2,2/x6/1,x5 1 21"
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.Black
        moveNumberRef <- newIORef 2
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        result <- MS.checkGameWin mutBoard B.Black
        result `shouldBe` B.Road B.Black
    describe "checkGameResult (mutable)" $ do
      it "detects non full board" $ do
        board <- VM.replicate (4 * 4) []
        result <- MS.checkFullBoard board False
        result `shouldBe` B.Continue
      it "detects full board" $ do
        board <- VM.replicate (4 * 4) [B.Piece B.White B.Flat]
        result <- MS.checkFullBoard board False
        result `shouldBe` B.FlatWin B.White
      it "detects when game is not over" $ do
        board <- VM.replicate (4 * 4) []
        turn <- newIORef B.White
        moveNumber <- newIORef 0
        player1 <- newIORef (B.Reserves 10 1)
        player2 <- newIORef (B.Reserves 10 1)
        resultRef <- newIORef B.Continue
        gameHistory <- newIORef []
        let gameState =
              MS.MGameState
                board
                turn
                moveNumber
                player1
                player2
                resultRef
                gameHistory
        result <- MS.checkGameResult gameState
        result `shouldBe` B.Continue
      it "detects a white road through mixed paths" $ do
        board <-
          MS.createMutableBoard $
          B.board $ TPS.parseTPSHard "x6/x6/x6/x6/x6/1,1,1,1,1,1 2 2"
        result <- MS.checkGameWin board B.White
        result `shouldBe` B.Road B.White
      it "detects a black road through mixed paths" $ do
        board <-
          MS.createMutableBoard $
          B.board $ TPS.parseTPSHard "2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2"
        result <- MS.checkGameWin board B.Black
        result `shouldBe` B.Road B.Black
      it "detects reserve game end" $ do
        board <- MS.createMutableBoard $ B.createEmptyBoard 6
        turn <- newIORef B.White
        moveNumber <- newIORef 0
        player1 <- newIORef (B.Reserves 0 0)
        player2 <- newIORef (B.Reserves 10 1)
        resultRef <- newIORef B.Continue
        gameHistory <- newIORef []
        let gameState =
              MS.MGameState
                board
                turn
                moveNumber
                player1
                player2
                resultRef
                gameHistory
        result <- MS.checkGameResult gameState
        result `shouldBe` B.Draw
    describe "move generation (mutable)" $ do
      it "should generate all valid first moves" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 1
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        moves <- MS.generateAllMoves gameState
        VM.length moves `shouldBe` 36
      it "should generate all valid placement moves for a player" $ do
        let board = B.createEmptyBoard 6
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 3
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        moves <- MS.generateAllMoves gameState
        VM.length moves `shouldBe` 108
      it "should generate all possible moves in a complex TPS position" $ do
        let tps =
              T.pack
                "2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/11,x5 1 2"
        let board = B.board $ TPS.parseTPSHard tps
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 3
        player1Ref <- newIORef (B.Reserves 21 1)
        player2Ref <- newIORef (B.Reserves 21 1)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        moves <- MS.generateAllMoves gameState
        VM.length moves `shouldBe` 18
      it
        "should generate all possible moves in a complex TPS position with capstones and standing stones" $ do
        let tps =
              T.pack
                "2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,2,1S/x2,2,1,21,1 1 42"
        let board = B.board $ TPS.parseTPSHard tps
        mutBoard <- MS.createMutableBoard board
        turnRef <- newIORef B.White
        moveNumberRef <- newIORef 42
        player1Ref <- newIORef (B.Reserves 21 0)
        player2Ref <- newIORef (B.Reserves 21 0)
        resultRef <- newIORef B.Continue
        historyRef <- newIORef []
        let gameState =
              MS.MGameState
                mutBoard
                turnRef
                moveNumberRef
                player1Ref
                player2Ref
                resultRef
                historyRef
        moves <- MS.generateAllMoves gameState
        let gs =
              B.GameState
                board
                B.White
                42
                (B.Reserves 21 0)
                (B.Reserves 21 0)
                B.Continue
                []
        let moves' = Moves.generateAllMoves gs
        putStrLn $ "Moves: " ++ show (length moves')
        v <- V.freeze moves
        let listMoves = V.toList v
        let diff = filter (\m -> notElem m moves') listMoves
        let diff' = filter (\m -> notElem m listMoves) moves'
        let diff'' = diff ++ diff'
        putStrLn $ "Moves: " ++ show diff''
        VM.length moves `shouldBe` 140
