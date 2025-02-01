{-# LANGUAGE OverloadedStrings #-}

module TestTPS where

import Board
import Data.Matrix
import TPS
import Test.Hspec

runTPSTests :: IO ()
runTPSTests =
  hspec $ do
    describe "TPS Parsing" $ do
      it "parses an empty 4x4 starting board correctly" $ do
        let tps = "x4/x4/x4/x4 1 1"
            gameState = parseTPSHard tps
        nrows (board gameState) `shouldBe` 4
        ncols (board gameState) `shouldBe` 4
        board gameState `shouldBe` createEmptyBoard 4
        turn gameState `shouldBe` White
        moveNumber gameState `shouldBe` 1
        player1 gameState `shouldBe` Reserves 15 0
        player2 gameState `shouldBe` Reserves 15 0
      it "parses a 5x5 starting board correctly" $ do
        let tps = "x5/x5/x5/x5/x5 1 1"
            gameState = parseTPSHard tps
        nrows (board gameState) `shouldBe` 5
        ncols (board gameState) `shouldBe` 5
        board gameState `shouldBe` createEmptyBoard 5
        turn gameState `shouldBe` White
        moveNumber gameState `shouldBe` 1
        player1 gameState `shouldBe` Reserves 21 1
        player2 gameState `shouldBe` Reserves 21 1
      it "removes TPS brackets and cleans input correctly" $ do
        let tps = "[TPS x4/x4/x4/x4 1 1]"
            gameState = parseTPSHard tps
        nrows (board gameState) `shouldBe` 4
        ncols (board gameState) `shouldBe` 4
        board gameState `shouldBe` createEmptyBoard 4
      it "parses turn and move number correctly" $ do
        let tps = "x4/x4/x4/x4 2 15"
            gameState = parseTPSHard tps
        turn gameState `shouldBe` Black
        moveNumber gameState `shouldBe` 15
      it "parses easy board" $ do
        let tps = "1C,x,212,x2/x5/x2,111,x,2S/x5/1S,x,121,x,2C 1 1"
        let gameState = parseTPSHard tps
        -- putStrLn $ show $ board gameState
        nrows (board gameState) `shouldBe` 5
        ncols (board gameState) `shouldBe` 5
        getElem 1 1 (board gameState) `shouldBe` [Piece White Standing]
        getElem 3 3 (board gameState) `shouldBe`
          [Piece White Flat, Piece White Flat, Piece White Flat]
        getElem 5 3 (board gameState) `shouldBe` [Piece Black Standing]
        getElem 5 1 (board gameState) `shouldBe` [Piece Black Cap]
        getElem 1 5 (board gameState) `shouldBe` [Piece White Cap]
      it "handles invalid TPS format (missing move number)" $ do
        let tps = "x4/x4/x4/x4 1"
        parseTPS tps `shouldBe` Prelude.Left (InvalidTPSFormat "x4/x4/x4/x4 1")
      it "handles invalid TPS format (extra component)" $ do
        let tps = "x4/x4/x4/x4 1 1 extra"
        parseTPS tps `shouldBe`
          Prelude.Left (InvalidTPSFormat "x4/x4/x4/x4 1 1 extra")
      it "handles invalid move number (non-numeric)" $ do
        let tps = "x4/x4/x4/x4 1 abc"
        parseTPS tps `shouldBe` Prelude.Left InvalidMoveNumber
      it "handles invalid piece (invalid character)" $ do
        let tps = "x4/x4/x4/x3,2Z 1 1"
        parseTPS tps `shouldBe` Prelude.Left (InvalidPiece 'Z')
      it "handles invalid piece (invalid character)" $ do
        let tps = "x4/x4/x4/x3,3C 1 1"
        parseTPS tps `shouldBe` Prelude.Left (InvalidPiece '3')
      it "handles invalid board size (inconsistent rows)" $ do
        let tps = "x4/x4/x4/x3 1 1"
        parseTPS tps `shouldBe` Prelude.Left (InvalidBoardSize 4)
      it "handles invalid square format (invalid square)" $ do
        let tps = "x4/x4/x4/x3,invalid 1 1"
        parseTPS tps `shouldBe` Prelude.Left (InvalidPiece 'i')
      it "handles empty input" $ do
        let tps = ""
        parseTPS tps `shouldBe` Prelude.Left (InvalidTPSFormat "")
    describe "Board to TPS Conversion" $ do
      it "converts an empty 4x4 board to TPS correctly" $ do
        let emptyBoard = createEmptyBoard 4
            gameState =
              GameState
                { board = emptyBoard
                , turn = White
                , moveNumber = 1
                , player1 = Reserves 15 0
                , player2 = Reserves 15 0
                , result = Nothing
                , gameHistory = []
                }
            tps = gameStateToTPS gameState
        tps `shouldBe` "x4/x4/x4/x4 1 1"
      it "converts a 6x6 board with pieces to TPS correctly" $ do
        let gs =
              parseTPSHard
                "2,1,2,2,2,1/2,2,2,21,2S,21/2,2,1S,11C,1,x/1112C,2,1,x3/1,221S,1,1,x2/x,1S,2,x3 2 24"
        gameStateToTPS gs `shouldBe`
          "2,1,2,2,2,1/2,2,2,21,2S,21/2,2,1S,11C,1,x/1112C,2,1,x3/1,221S,1,1,x2/x,1S,2,x3 2 24"
      it "converts a 6x6 board with pieces to TPS correctly" $ do
        let gs =
              parseTPSHard
                "1,2,1,2,1,1/2,1,1,2S,1,1/2,x,1,1121S,2S,1/2,211112C,221C,2S,1112,x/x,22,21,2S,1,212S/2,1,1,2,2S,22121S 2 41"
        gameStateToTPS gs `shouldBe`
          "1,2,1,2,1,1/2,1,1,2S,1,1/2,x,1,1121S,2S,1/2,211112C,221C,2S,1112,x/x,22,21,2S,1,212S/2,1,1,2,2S,22121S 2 41"
