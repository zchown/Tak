{-# LANGUAGE OverloadedStrings #-}

module TestMoves where

import Board
import Data.Either (fromRight)
import Data.Matrix
import qualified Data.Text as T
import Moves
import TPS
import Test.Hspec

runMoveTests :: IO ()
runMoveTests =
  hspec $ do
    describe "Move Validation" $ do
      it "should allow placing a flat stone on an empty square" $ do
        let b = createEmptyBoard 5
            move = PlaceFlat (Position (3, 3), White)
        checkMove b move `shouldBe` Prelude.Right True
      it "should reject placing a flat stone on an occupied square" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = PlaceFlat (Position (3, 3), Black)
        checkMove b move `shouldBe` Prelude.Left (InvalidMove "Square Occupied")
      it "should allow sliding a stack within the board boundaries" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 1, Up, [1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "should reject sliding a stack off the board" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (1, 1)) White
            move = Slide (Position (1, 1), 1, Down, [1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Space Down")
      it "should reject slide when a capstone is in the way" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeCap b (Position (2, 3)) White
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe` Prelude.Left (InvalidMove "Cap In The Way")
      it "should reject slide when a standing piece is in the way" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) White
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "basic capstone slide" $ do
        let b = placeCap (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 1, Up, [1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "capstone with crush" $ do
        let b = placeCap (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) Black
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, True)
        checkMove b' move `shouldBe` Prelude.Right True
      it "capstone crush set incorrectly" $ do
        let b = placeCap (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) Black
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe`
          Prelude.Left (InvalidMove "Crush not set correctly")
      it "should reject slide with invalid drop counts" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 2, Up, [1, 0], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count or Drops")
      it "should reject slide with insufficient pieces in stack" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 2, Up, [1, 1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count For Stack")
    describe "Making Moves" $ do
      it "should place a flat stone on the board" $ do
        let b = createEmptyBoard 5
            move = PlaceFlat (Position (3, 3), White)
            newb = makeMove b move
        newb `shouldBe` Prelude.Right (placeFlat b (Position (3, 3)) White)
      it "should slide a stack to an adjacent square" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
            newb = makeMove b move
        newb `shouldBe`
          Prelude.Right (placeFlat (createEmptyBoard 5) (Position (2, 3)) White)
      it "should handle complex slides with multiple drops" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeFlat b (Position (2, 3)) White
            setUpMove =
              Slide (Position (2, 3), 1, Board.Right, [1], White, False)
            b'' =
              case makeMove b' setUpMove of
                Prelude.Right nb -> nb
                Prelude.Left _ -> createEmptyBoard 5
            move = Slide (Position (3, 3), 2, Board.Left, [1, 1], White, False)
            newb = makeMove b'' move
        newb `shouldBe`
          Prelude.Right
            (placeFlat
               (placeFlat (createEmptyBoard 5) (Position (2, 3)) White)
               (Position (1, 3))
               White)
      it "reject slides that would exceed board boundaries with multiple drops" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x4,111/x5/x5 1 1]"
        let move = Slide (Position (5, 3), 3, Board.Right, [1, 2], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Space Right")
      it "should handle slides with standing stones in the path" $ do
        let b =
              board (parseTPSHard (T.pack "[TPS x5/x5/x2,111,x,2S/x5/x5 2 1]"))
        let move = Slide (Position (3, 3), 3, Board.Right, [1, 2], White, False)
        getElem 3 3 b `shouldBe`
          [Piece White Flat, Piece White Flat, Piece White Flat]
        getElem 5 3 b `shouldBe` [Piece Black Standing]
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should allow slides that crush standing stones with a capstone" $ do
        let b =
              board $ parseTPSHard $ T.pack "[TPS x5/x5/x2,111C,x,2S/x5/x5 1 2]"
        let move = Slide (Position (3, 3), 3, Board.Right, [2, 1], White, True)
        getElem 3 3 b `shouldBe`
          [Piece White Cap, Piece White Flat, Piece White Flat]
        getElem 5 3 b `shouldBe` [Piece Black Standing]
        checkMove b move `shouldBe` Prelude.Right True
      it "should not allow crush with flat and cap stone" $ do
        let b =
              board $ parseTPSHard $ T.pack "[TPS x5/x5/x2,111C,x,2S/x5/x5 1 2]"
        let move = Slide (Position (3, 3), 3, Board.Right, [1, 2], White, True)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
    describe "Undo Moves" $ do
      it "should undo a flat stone placement" $ do
        let b = createEmptyBoard 5
            move = PlaceFlat (Position (3, 3), White)
            newb = fromRight b (makeMove b move)
            undoneb = undoMove newb move
        undoneb `shouldBe` Prelude.Right b
      it "should undo a standing stone placement" $ do
        let b = createEmptyBoard 5
            move = PlaceStanding (Position (3, 3), White)
            newb = fromRight b (makeMove b move)
            undoneb = undoMove newb move
        undoneb `shouldBe` Prelude.Right b
      it "should undo a capstone placement" $ do
        let b = createEmptyBoard 5
            move = PlaceCap (Position (3, 3), White)
            newb = fromRight b (makeMove b move)
            undoneb = undoMove newb move
        undoneb `shouldBe` Prelude.Right b
      it "should undo a slide move" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 1, Up, [1], White, False)
            newb = fromRight b (makeMove b move)
            undoneb = undoMove newb move
        undoneb `shouldBe` Prelude.Right b
      it "should reject undoing a slide with invalid drop counts" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x4,111/x5/x5 2 1]"
            move = Slide (Position (3, 4), 4, Board.Left, [4], White, False)
        undoMove b move `shouldBe`
          Prelude.Left (InvalidSlideUndo "Not Enough Pieces")
      it "should reject undoing a slide with invalid drop counts" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x4,111/x5/x5 2 1]"
            move = Slide (Position (3, 4), 4, Board.Left, [1, 2], White, False)
        undoMove b move `shouldBe`
          Prelude.Left (InvalidSlideUndo "Drop Count Mismatch")
      it "should reject undoing a slide move with impossible drops" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x3,111,x/x5/x5 2 1]"
            move = Slide (Position (3, 3), 3, Board.Right, [1, 2], White, False)
        undoMove b move `shouldBe`
          Prelude.Left (InvalidSlideUndo "Not Enough Pieces")
      it "should reject undoing a slide with beyond limit drops" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x4,1111111/x5/x5 2 1]"
            move = Slide (Position (3, 4), 6, Board.Left, [6], White, False)
        undoMove b move `shouldBe`
          Prelude.Left (InvalidSlideUndo "Invalid Count")
      it "should reject undoing a slide with insufficient pieces" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 1, Up, [1], White, False)
            newb = fromRight b (makeMove b move)
            invalidMove = Slide (Position (3, 3), 2, Up, [1, 1], White, False)
            undoneb = undoMove newb invalidMove
        undoneb `shouldBe` Prelude.Left (InvalidSlideUndo "Not Enough Pieces")
      it "correctly undo more complex slide move" $ do
        let b =
              board $
              parseTPSHard
                "221,x4,2/x5,1C/1,21,x,2,x,2/1,x2,2,2C,1S/2S,21,x4/1,x4,2 2 12"
        let move = Slide (Position (1, 5), 2, Board.Up, [2], White, False)
        case undoMove b move of
          Prelude.Left _ -> expectationFailure "Undo failed"
          Prelude.Right b' -> do
            getElem 1 5 b' `shouldBe` [Piece White Flat, Piece Black Flat]
            getElem 1 6 b' `shouldBe` [Piece Black Flat]
      it "correctly undo more complex slide move" $ do
        let b =
              board $
              parseTPSHard
                "221,x4,2/2S,1,x3,1C/1,21,x,2,x,2/1,12C,2,x2,1S/2S,21,x4/1,x4,2 1 15"
        let move = Slide (Position (4, 3), 2, Board.Left, [1, 1], Black, False)
        case undoMove b move of
          Prelude.Left _ -> expectationFailure "Undo failed"
          Prelude.Right b' -> do
            getElem 4 3 b' `shouldBe` [Piece Black Cap, Piece Black Flat]
            getElem 3 3 b' `shouldBe` []
            getElem 2 3 b' `shouldBe` [Piece White Flat]
      it "correctly undo more complex slide move" $ do
        let b =
              board $
              parseTPSHard
                "2212S,1112C,1S,2,2,2/1S,12,x,2,x,2/121,1S,2,x,221C,x/21S,1,x,2221,x,1S/2S,21,221S,1S,1,x/1,1,2,2,2,2 1 35"
        let move = Slide (Position (2, 5), 4, Board.Up, [4], Black, False)
        case undoMove b move of
          Prelude.Left _ -> expectationFailure "Undo failed"
          Prelude.Right b' -> do
            getElem 2 5 b' `shouldBe`
              [ Piece Black Cap
              , Piece White Flat
              , Piece White Flat
              , Piece White Flat
              , Piece Black Flat
              , Piece White Flat
              ]
            getElem 2 6 b' `shouldBe` []
      it "undo crush" $ do
        let b =
              board $
              parseTPSHard
                "x5,1/x4,1,x/2,x,1,1,1212C,x/1,21,221C,2S,1112,x/2,2,2,1,1,2/2,x,1,x,1,2 1 21"
        let move = Slide (Position (4, 4), 1, Board.Right, [1], Black, True)
        case undoMove b move of
          Prelude.Left _ -> expectationFailure "Undo failed"
          Prelude.Right b' -> do
            getElem 4 4 b' `shouldBe` [Piece Black Cap, Piece White Flat]
            getElem 5 4 b' `shouldBe`
              [Piece White Standing, Piece Black Flat, Piece White Flat]
    describe "Move Generation" $ do
      it "should generate all valid first moves" $ do
        let gs =
              GameState
                (createEmptyBoard 5)
                White
                1
                (Reserves 21 1)
                (Reserves 21 1)
                Continue
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
                Continue
                []
            moves = generateAllMoves gs
        length moves `shouldBe` 75
      it "should generate all valid slide moves for a player" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            moves = slideMoves b White
        length moves `shouldBe` 4
      it "should generate valid slides for a stack of stones" $ do
        let b = board $ parseTPSHard $ T.pack "[TPS x5/x5/x2,11,x2/x5/x5 1 1]"
            moves = slideMoves b White
        length moves `shouldBe` 12
      it "should generate valid slides with crushing for a capstone" $ do
        let b = placeCap (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) Black
            moves = slideMoves b' White
        length moves `shouldBe` 4
    describe "Edge Cases" $ do
      it "should reject placing a stone outside the board boundaries" $ do
        let b = createEmptyBoard 5
            move = PlaceFlat (Position (0, 3), White)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Position (1, 1) or greater")
      it "should reject sliding a stack outside the board boundaries" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (1, 1)) White
            move = Slide (Position (1, 1), 1, Board.Left, [1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Not Enough Space Left")
      it "should reject sliding a stack with invalid drop counts" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 2, Up, [1, 2], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count or Drops")
      it "should reject sliding a stack with insufficient pieces" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            move = Slide (Position (3, 3), 2, Up, [1, 1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Invalid Count For Stack")
      it "should reject sliding a stack with a standing stone in the way" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) Black
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should reject sliding a stack with a capstone in the way" $ do
        let b = placeFlat (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeCap b (Position (2, 3)) White
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe` Prelude.Left (InvalidMove "Cap In The Way")
      it "should reject sliding a stack with incorrect crush setting" $ do
        let b = placeCap (createEmptyBoard 5) (Position (3, 3)) White
            b' = placeStanding b (Position (2, 3)) Black
            move = Slide (Position (3, 3), 1, Board.Left, [1], White, False)
        checkMove b' move `shouldBe`
          Prelude.Left (InvalidMove "Crush not set correctly")
    describe "Complex TPS Positions" $ do
      it "should handle a complex TPS position with multiple stacks" $ do
        let tps = T.pack "[TPS x5/x5/1,2,x,2,1/x5/x5 1 1]"
            b = board $ parseTPSHard tps
            move = Slide (Position (1, 3), 1, Board.Right, [1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "should handle a TPS position with a capstone and standing stones" $ do
        let tps = T.pack "[TPS x5/x5/1C,2S,x,2,1/x5/x5 1 1]"
            b = board $ parseTPSHard tps
            move = Slide (Position (1, 3), 1, Board.Right, [1], White, True)
        checkMove b move `shouldBe` Prelude.Right True
      it
        "should reject a slide in a TPS position with a standing stone in the way" $ do
        let tps = T.pack "[TPS x5/x5/1,2S,x,2,1/x5/x5 1 1]"
            b = board $ parseTPSHard tps
            move = Slide (Position (1, 3), 1, Board.Right, [1], White, False)
        checkMove b move `shouldBe`
          Prelude.Left (InvalidMove "Standing In The Way")
      it "should handle a TPS position with a complex slide and multiple drops" $ do
        let tps = T.pack "[TPS x5/x5/121,2,2,2,1/x5/x5 1 1]"
            b = board $ parseTPSHard tps
            move =
              Slide (Position (1, 3), 3, Board.Right, [1, 1, 1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
      it "should reject a slide in a TPS position with insufficient pieces" $ do
        let tps = T.pack "[TPS x5/x5/2121C,2,2,2,1/x5/x5 1 1]"
            b = board $ parseTPSHard tps
            move =
              Slide
                (Position (1, 3), 4, Board.Right, [1, 1, 1, 1], White, False)
        checkMove b move `shouldBe` Prelude.Right True
