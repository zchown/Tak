{-# LANGUAGE OverloadedStrings #-}

module TestMoves where

import Board
import Data.Matrix
import Moves
import TPS
import Test.Hspec

runMoveTests :: IO ()
runMoveTests =
  hspec $ do
    describe "Move Validation" $ do
      it "should allow placing a flat stone on an empty square" $ do
        let board = createEmptyBoard 5
            move = PlaceFlat (Position 3 3, White)
        checkMove board move `shouldBe` Prelude.Right True
      it "should reject placing a flat stone on an occupied square" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = PlaceFlat (Position 3 3, Black)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Square Occupied")
      it "should allow sliding a stack within the board boundaries" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board move `shouldBe` Prelude.Right True
      it "should reject sliding a stack off the board" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 1 1) White
            move = Slide (Position 1 1, 1, Up, [1], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Rows Up")
      it "should reject slide when a capstone is in the way" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeCap board (Position 2 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Cap In The Way")
      it "should reject slide when a standing piece is in the way" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "basic capstone slide" $ do
        let board = placeCap (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board move `shouldBe` Prelude.Right True
      it "capstone with crush" $ do
        let board = placeCap (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) Black
            move = Slide (Position 3 3, 1, Up, [1], White, True)
        checkMove board' move `shouldBe` Prelude.Right True
      it "capstone crush set incorrectly" $ do
        let board = placeCap (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) Black
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Crush not set correctly")
    describe "Making Moves" $ do
      it "should place a flat stone on the board" $ do
        let board = createEmptyBoard 5
            move = PlaceFlat (Position 3 3, White)
            newBoard = makeMove board move
        newBoard `shouldBe` Prelude.Right (placeFlat board (Position 3 3) White)
      it "should slide a stack to an adjacent square" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
            newBoard = makeMove board move
        newBoard `shouldBe`
          Prelude.Right (placeFlat (createEmptyBoard 5) (Position 2 3) White)
    describe "Undoing Moves" $ do
      it "should undo placing a flat stone" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = PlaceFlat (Position 3 3, White)
            undoneBoard = undoMove board move
        undoneBoard `shouldBe` Prelude.Right (createEmptyBoard 5)
      it "should undo sliding a stack" $ do
        let board =
              placeFlat
                (placeFlat (createEmptyBoard 5) (Position 3 3) White)
                (Position 2 3)
                White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
            undoneBoard = undoMove board move
        undoneBoard `shouldBe`
          Prelude.Right (placeFlat (createEmptyBoard 5) (Position 3 3) White)
    describe "Move Generation" $ do
      it "should generate all valid placement moves for the first move" $ do
        let gameState =
              GameState
                (createEmptyBoard 5)
                White
                1
                (Reserves 21 1)
                (Reserves 21 1)
                Nothing
                []
            moves = generateAllMoves gameState
        length moves `shouldBe` 25
      it "should generate all valid placement moves for non first move" $ do
        let gameState =
              GameState
                (createEmptyBoard 5)
                White
                2
                (Reserves 21 1)
                (Reserves 21 1)
                Nothing
                []
            moves = generateAllMoves gameState
        length moves `shouldBe` 75
      it "should generate all valid slide moves for a controlled stack" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            gameState =
              GameState board White 2 (Reserves 21 1) (Reserves 21 1) Nothing []
            moves = generateAllMoves gameState
            m' =
              filter
                (\m ->
                   case m of
                     Slide _ -> True
                     _ -> False)
                moves
        length m' `shouldBe` 4
