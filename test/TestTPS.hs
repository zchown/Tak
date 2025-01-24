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
      it "removes tps" $ do
        let tps = "[TPS x4/x4/x4/x4 1 1]"
            gameState = parseTPS tps
        nrows (board gameState) `shouldBe` 4
        ncols (board gameState) `shouldBe` 4
        board gameState `shouldBe` createEmptyBoard 4
      it "parses turn and move number correctly" $ do
        let tps = "x4/x4/x4/x4 2 15"
            gameState = parseTPS tps
        turn gameState `shouldBe` Black
        moveNumber gameState `shouldBe` 15
      it "parses a board with pieces correctly" $ do
        let tps =
              "x3,12,2S/x,22S,22C,11,21/121,212,12,1121C,1212S/21S,1,21,211S,12S/x,21S,2,x2 1 26"
            gameState = parseTPS tps
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
