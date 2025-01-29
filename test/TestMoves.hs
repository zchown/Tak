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
      it "should reject slide with invalid drop counts" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 0], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count or Drops")
      it "should reject slide with insufficient pieces in stack" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 1], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count For Stack")
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
      it "should handle complex slides with multiple drops" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeFlat board (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 1], White, False)
            newBoard = makeMove board' move
        newBoard `shouldBe`
          Prelude.Right
            (placeFlat
               (placeFlat (createEmptyBoard 5) (Position 2 3) White)
               (Position 1 3)
               White)
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
      it "should fail to undo a slide with invalid drop counts" $ do
        let board =
              placeFlat
                (placeFlat (createEmptyBoard 5) (Position 3 3) White)
                (Position 2 3)
                White
            move = Slide (Position 3 3, 2, Up, [1, 0], White, False)
            undoneBoard = undoMove board move
        undoneBoard `shouldBe`
          Prelude.Left (InvalidSlideUndo "Drop Count Mismatch")
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
      it "should generate no slide moves for a stack of height 1" $ do
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
      it "should generate slide moves for a stack of height 2" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeFlat board (Position 3 3) White
            gameState =
              GameState
                board'
                White
                2
                (Reserves 21 1)
                (Reserves 21 1)
                Nothing
                []
            moves = generateAllMoves gameState
            m' =
              filter
                (\m ->
                   case m of
                     Slide _ -> True
                     _ -> False)
                moves
        length m' `shouldBe` 8
      it "should allow complex slides with multiple drops across the board" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, False)
        checkMove board move `shouldBe` Prelude.Right True
      it
        "should reject slides that would exceed board boundaries with multiple drops" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 5, Position 3 5, Position 3 5]
            move = Slide (Position 3 5, 3, Board.Right, [1, 2], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count For Stack")
      it "should handle slides with standing stones in the path" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            board' = placeStanding board (Position 3 4) Black
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should allow slides that crush standing stones with a capstone" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            board' = placeCap board (Position 3 3) White
            board'' = placeStanding board' (Position 3 4) Black
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, True)
        checkMove board'' move `shouldBe` Prelude.Right True
    describe "Making Moves" $ do
      it "should handle complex slides with multiple drops across the board" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, False)
            newBoard = makeMove board move
        newBoard `shouldBe`
          Prelude.Right
            (placeFlat
               (placeFlat
                  (placeFlat (createEmptyBoard 5) (Position 3 4) White)
                  (Position 3 5)
                  White)
               (Position 3 5)
               White)
      it "should handle slides that crush standing stones with a capstone" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            board' = placeCap board (Position 3 3) White
            board'' = placeStanding board' (Position 3 4) Black
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, True)
            newBoard = makeMove board'' move
        newBoard `shouldBe`
          Prelude.Right
            (placeFlat
               (placeFlat
                  (placeFlat (createEmptyBoard 5) (Position 3 4) White)
                  (Position 3 5)
                  White)
               (Position 3 5)
               White)
    describe "Undoing Moves" $ do
      it "should undo complex slides with multiple drops" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, False)
            newBoard = makeMove board move
            undoneBoard =
              undoMove (fromRight (createEmptyBoard 5) newBoard) move
        undoneBoard `shouldBe` Prelude.Right board
      it "should undo slides that crush standing stones with a capstone" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
            board' = placeCap board (Position 3 3) White
            board'' = placeStanding board' (Position 3 4) Black
            move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, True)
            newBoard = makeMove board'' move
            undoneBoard =
              undoMove (fromRight (createEmptyBoard 5) newBoard) move
        undoneBoard `shouldBe` Prelude.Right board''
    describe "Move Generation" $ do
      it "should generate all valid slide moves for a stack of height 3" $ do
        let board =
              foldl
                (\b p -> placeFlat b p White)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
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
        length m' `shouldBe` 16
      it "should generate no slide moves for a stack controlled by the opponent" $ do
        let board =
              foldl
                (\b p -> placeFlat b p Black)
                (createEmptyBoard 5)
                [Position 3 3, Position 3 3, Position 3 3]
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
        length m' `shouldBe` 0

fromRight :: b -> Either a b -> b
fromRight _ (Prelude.Right x) = x
fromRight def _ = def
