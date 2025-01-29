{-# LANGUAGE OverloadedStrings #-}

module TestMoves where

import Board
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as T
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
            board' = placeFlat board (Position 2 3) White
            setUpMove = Slide (Position 2 3, 1, Down, [1], White, False)
            board'' =
              case makeMove board' setUpMove of
                Prelude.Right b -> b
                Prelude.Left _ -> createEmptyBoard 5
            move = Slide (Position 3 3, 2, Up, [1, 1], White, False)
            newBoard = makeMove board'' move
        newBoard `shouldBe`
          Prelude.Right
            (placeFlat
               (placeFlat (createEmptyBoard 5) (Position 2 3) White)
               (Position 1 3)
               White)
      it "reject slides that would exceed board boundaries with multiple drops" $ do
        let b = board $ parseTPS $ T.pack "[TPS x5/x5/x4,111/x5/x5 1 1]"
        let move = Slide (Position 3 5, 3, Board.Right, [1, 2], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Columns Right")
      it "should handle slides with standing stones in the path" $ do
        let b = board (parseTPS (T.pack "[TPS x5/x5/x2,111,x,2S/x5/x5 2 1]"))
        let move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should allow slides that crush standing stones with a capstone" $ do
        let b = board $ parseTPS $ T.pack "[TPS x5/x5/x2,111C,x,2S/x5/x5 1 2]"
        let move = Slide (Position 3 3, 3, Board.Right, [2, 1], White, True)
        checkMove b move `shouldBe` Prelude.Right True
      it "should not allow crush with flat and cap stone" $ do
        let b = board $ parseTPS $ T.pack "[TPS x5/x5/x2,111C,x,2S/x5/x5 1 2]"
        let move = Slide (Position 3 3, 3, Board.Right, [1, 2], White, True)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
    describe "Undo Moves" $ do
      it "should undo a flat stone placement" $ do
        let board = createEmptyBoard 5
            move = PlaceFlat (Position 3 3, White)
            newBoard = fromRight board (makeMove board move)
            undoneBoard = undoMove newBoard move
        undoneBoard `shouldBe` Prelude.Right board
      it "should undo a standing stone placement" $ do
        let board = createEmptyBoard 5
            move = PlaceStanding (Position 3 3, White)
            newBoard = fromRight board (makeMove board move)
            undoneBoard = undoMove newBoard move
        undoneBoard `shouldBe` Prelude.Right board
      it "should undo a capstone placement" $ do
        let board = createEmptyBoard 5
            move = PlaceCap (Position 3 3, White)
            newBoard = fromRight board (makeMove board move)
            undoneBoard = undoMove newBoard move
        undoneBoard `shouldBe` Prelude.Right board
      it "should undo a slide move" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
            newBoard = fromRight board (makeMove board move)
            undoneBoard = undoMove newBoard move
        undoneBoard `shouldBe` Prelude.Right board
      it "should reject undoing a slide with invalid drop counts" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 1], White, False)
            newBoard = fromRight board (makeMove board move)
            invalidMove = Slide (Position 3 3, 2, Up, [2], White, False)
            undoneBoard = undoMove newBoard invalidMove
        undoneBoard `shouldBe`
          Prelude.Left (InvalidSlideUndo "Drop Count Mismatch")
      it "should reject undoing a slide with insufficient pieces" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
            newBoard = fromRight board (makeMove board move)
            invalidMove = Slide (Position 3 3, 2, Up, [1, 1], White, False)
            undoneBoard = undoMove newBoard invalidMove
        undoneBoard `shouldBe`
          Prelude.Left (InvalidSlideUndo "Not Enough Pieces")
    describe "Move Generation" $ do
      it "should generate all valid first moves" $ do
        let gs =
              GameState
                (createEmptyBoard 5)
                White
                1
                (Reserves 21 1)
                (Reserves 21 1)
                Nothing
                []
            moves = generateAllMoves gs
        length moves `shouldBe` 25
      it "should generate all valid placement moves for a player" $ do
        let gs =
              GameState
                (createEmptyBoard 5)
                White
                2
                (Reserves 21 1)
                (Reserves 21 1)
                Nothing
                []
            moves = generateAllMoves gs
        length moves `shouldBe` 75
      it "should generate all valid slide moves for a player" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            gs = GameState board White 2 (Reserves 21 1) (Reserves 21 1)
            moves = slideMoves board White
        length moves `shouldBe` 4
      it "should generate valid slides for a stack of stones" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeFlat board (Position 3 3) White
            gs = GameState board' White 2 (Reserves 21 1) (Reserves 21 1)
            moves = slideMoves board' White
        length moves `shouldBe` 8
      it "should generate valid slides with crushing for a capstone" $ do
        let board = placeCap (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) Black
            moves = slideMoves board' White
        length moves `shouldBe` 4
    describe "Edge Cases" $ do
      it "should reject placing a stone outside the board boundaries" $ do
        let board = createEmptyBoard 5
            move = PlaceFlat (Position 0 3, White)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Position (1, 1) or greater")
      it "should reject sliding a stack outside the board boundaries" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 1 1) White
            move = Slide (Position 1 1, 1, Board.Left, [1], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Columns Left")
      it "should reject sliding a stack with invalid drop counts" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 2], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count or Drops")
      it "should reject sliding a stack with insufficient pieces" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            move = Slide (Position 3 3, 2, Up, [1, 1], White, False)
        checkMove board move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count For Stack")
      it "should reject sliding a stack with a standing stone in the way" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) Black
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should reject sliding a stack with a capstone in the way" $ do
        let board = placeFlat (createEmptyBoard 5) (Position 3 3) White
            board' = placeCap board (Position 2 3) White
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Cap In The Way")
      it "should reject sliding a stack with incorrect crush setting" $ do
        let board = placeCap (createEmptyBoard 5) (Position 3 3) White
            board' = placeStanding board (Position 2 3) Black
            move = Slide (Position 3 3, 1, Up, [1], White, False)
        checkMove board' move `shouldBe`
          Prelude.Left (InvalidMove "Crush not set correctly")
    describe "Complex TPS Positions" $ do
      it "should handle a complex TPS position with multiple stacks" $ do
        let tps = T.pack "[TPS x5/x5/1,2,x,2,1/x5/x5 1 1]"
            b = board $ parseTPS tps
            move = Slide (Position 3 1, 1, Board.Right, [1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "should handle a TPS position with a capstone and standing stones" $ do
        let tps = T.pack "[TPS x5/x5/1C,2S,x,2,1/x5/x5 1 1]"
            b = board $ parseTPS tps
            move = Slide (Position 3 1, 1, Board.Right, [1], White, True)
        checkMove b move `shouldBe` Prelude.Right True
      it
        "should reject a slide in a TPS position with a standing stone in the way" $ do
        let tps = T.pack "[TPS x5/x5/1,2S,x,2,1/x5/x5 1 1]"
            b = board $ parseTPS tps
            move = Slide (Position 3 1, 1, Board.Right, [1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should handle a TPS position with a complex slide and multiple drops" $ do
        let tps = T.pack "[TPS x5/x5/1,2,2,2,1/x5/x5 1 1]"
            b = board $ parseTPS tps
            move = Slide (Position 3 1, 3, Board.Right, [1, 1, 1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "should reject a slide in a TPS position with insufficient pieces" $ do
        let tps = T.pack "[TPS x5/x5/1,2,2,2,1/x5/x5 1 1]"
            b = board $ parseTPS tps
            move =
              Slide (Position 3 1, 4, Board.Right, [1, 1, 1, 1], White, False)
        checkMove b move `shouldBe` Prelude.Right True

fromRight :: b -> Either a b -> b
fromRight _ (Prelude.Right x) = x
fromRight def _ = def
