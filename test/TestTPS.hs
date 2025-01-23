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
            gameState = parseTPS tps
        nrows (board gameState) `shouldBe` 4
        ncols (board gameState) `shouldBe` 4
        board gameState `shouldBe` createEmptyBoard 4
        turn gameState `shouldBe` White
        moveNumber gameState `shouldBe` 1
        player1 gameState `shouldBe` Reserves 15 0
        player2 gameState `shouldBe` Reserves 15 0
      it "parses a 5x5 starting board correctly" $ do
        let tps = "x5/x5/x5/x5/x5 1 1"
            gameState = parseTPS tps
        nrows (board gameState) `shouldBe` 5
        ncols (board gameState) `shouldBe` 5
        board gameState `shouldBe` createEmptyBoard 5
        turn gameState `shouldBe` White
        moveNumber gameState `shouldBe` 1
        player1 gameState `shouldBe` Reserves 21 1
        player2 gameState `shouldBe` Reserves 21 1
      it "parses a board with mixed stone types in correct order" $ do
        let tps = "1,1S,1C,2,2S/x4/2,2,2,1,1 2 15"
            gameState = parseTPS tps
        let firstRowSquare = getElem 1 1 (board gameState)
        firstRowSquare `shouldSatisfy` (not . null)
        head firstRowSquare `shouldBe` Piece White Cap
        firstRowSquare !! 1 `shouldBe` Piece White Standing
        firstRowSquare !! 2 `shouldBe` Piece White Flat
      it "handles multiple empty squares condensed notation" $ do
        let tps = "1,x2,1/x4/2,x2,2 1 10"
            gameState = parseTPS tps
        let firstRowSquare = getElem 1 1 (board gameState)
        firstRowSquare `shouldBe` [Piece White Flat]
        getElem 1 2 (board gameState) `shouldBe` []
        getElem 1 3 (board gameState) `shouldBe` []
        getElem 1 4 (board gameState) `shouldBe` [Piece White Flat]
      it "calculates reserves correctly for different board sizes" $ do
        let tps4 = "1,1,1/x4 1 1"
            gameState4 = parseTPS tps4
        player1 gameState4 `shouldBe` Reserves 12 0
        let tps5 = "1,1,1,1,1/x4 1 1"
            gameState5 = parseTPS tps5
        player1 gameState5 `shouldBe` Reserves 16 1
      it "handles stacked stones with correct bottom-to-top ordering" $ do
        let tps = "11,12,13/x4 1 7"
            gameState = parseTPS tps
        let stack = getElem 1 1 (board gameState)
        length stack `shouldBe` 3
        stack !! 0 `shouldBe` Piece White Flat
        stack !! 1 `shouldBe` Piece White Flat
        stack !! 2 `shouldBe` Piece White Flat
      it "parses turn and move number correctly" $ do
        let tps = "x4/x4/x4/x4 2 15"
            gameState = parseTPS tps
        turn gameState `shouldBe` Black
        moveNumber gameState `shouldBe` 15
