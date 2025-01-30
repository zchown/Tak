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
      it "parses a board with pieces correctly" $ do
        let tps =
              "x3,12,2S/x,22S,22C,11,21/121,212,12,1121C,1212S/21S,1,21,211S,12S/x,21S,2,x2 1 26"
            gameState = parseTPSHard tps
        nrows (board gameState) `shouldBe` 5
        ncols (board gameState) `shouldBe` 5
        board gameState `shouldBe`
          fromList
            5
            5
            [ []
            , []
            , []
            , [Piece Black Flat, Piece White Flat]
            , [Piece Black Standing]
            , []
            , [Piece Black Standing, Piece Black Flat]
            , [Piece Black Cap, Piece Black Flat]
            , [Piece White Flat, Piece White Flat]
            , [Piece White Flat, Piece Black Flat]
            , [Piece White Flat, Piece Black Flat, Piece White Flat]
            , [Piece Black Flat, Piece White Flat, Piece Black Flat]
            , [Piece Black Flat, Piece White Flat]
            , [ Piece White Cap
              , Piece Black Flat
              , Piece White Flat
              , Piece White Flat
              ]
            , [ Piece Black Standing
              , Piece White Flat
              , Piece Black Flat
              , Piece White Flat
              ]
            , [Piece White Standing, Piece Black Flat]
            , [Piece White Flat]
            , [Piece White Flat, Piece Black Flat]
            , [Piece White Standing, Piece White Flat, Piece Black Flat]
            , [Piece Black Standing, Piece White Flat]
            , []
            , [Piece White Standing, Piece Black Flat]
            , [Piece Black Flat]
            , []
            , []
            ]
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
