module TestBoard where

import Board
import Data.Matrix
import Test.Hspec

runBoardTests :: IO ()
runBoardTests =
  hspec $ do
    describe "Board Module" $ do
      describe "createEmptyBoard" $ do
        it "creates a 4x4 board" $ do
          let board = createEmptyBoard 4
          nrows board `shouldBe` 4
          ncols board `shouldBe` 4
          all null (toList board) `shouldBe` True
      describe "getTopPiece" $ do
        it "returns Nothing for an empty square" $ do
          let board = createEmptyBoard 4
          getTopPiece 1 1 board `shouldBe` Nothing
        it "returns the top piece of a non-empty stack" $ do
          let board = setElem [Piece White Flat] (1, 1) (createEmptyBoard 4)
          getTopPiece 1 1 board `shouldBe` Just (Piece White Flat)
      describe "addToStack" $ do
        it "adds a piece to an empty stack" $ do
          addToStack [] (Piece White Flat) `shouldBe` Just [Piece White Flat]
        it "prevents adding to a non-flat top piece" $ do
          let stack = [Piece White Standing]
          addToStack stack (Piece Black Flat) `shouldBe` Nothing
      describe "getAllPieces" $ do
        it "returns all pieces of a specific color" $ do
          let board =
                fromLists
                  [ [[Piece White Flat], [Piece Black Standing]]
                  , [[Piece White Cap], [Piece Black Flat]]
                  ]
          getAllPieces board White `shouldBe`
            [Piece White Flat, Piece White Cap]
      describe "getPlaced" $ do
        it "correctly counts placed pieces" $ do
          let board =
                fromLists
                  [ [[Piece White Flat], [Piece Black Standing]]
                  , [[Piece White Cap], [Piece Black Flat]]
                  ]
          getPlaced board White `shouldBe` Reserves {stones = 1, caps = 1}
      describe "letterToCol and colToLetter" $ do
        it "converts between letters and column numbers" $ do
          letterToCol 'A' `shouldBe` 1
          letterToCol 'B' `shouldBe` 2
          colToLetter 1 `shouldBe` 'A'
          colToLetter 2 `shouldBe` 'B'
      describe "checkGameResult" $ do
        it "handles 4x4 full board draw" $ do
          let fullBoard = matrix 4 4 (const [Piece White Flat])
              gameState =
                GameState
                  { board = fullBoard
                  , turn = White
                  , moveNumber = 0
                  , player1 = Reserves 0 0
                  , player2 = Reserves 0 0
                  , result = Nothing
                  , gameHistory = []
                  }
          checkGameResult gameState `shouldBe` Just Draw
        it "detects non full board" $ do
          let partialBoard = matrix 4 4 (const [])
          checkFullBoard partialBoard `shouldBe` Nothing
        it "detects full board" $ do
          let partialBoard = matrix 4 4 (const [Piece White Flat])
          checkFullBoard partialBoard `shouldBe` Just (Win White)
        it "detects when game is not over" $ do
          let partialBoard = matrix 4 4 (const [])
              gameState =
                GameState
                  { board = partialBoard
                  , turn = White
                  , moveNumber = 0
                  , player1 = Reserves 10 1
                  , player2 = Reserves 10 1
                  , result = Nothing
                  , gameHistory = []
                  }
          checkGameResult gameState `shouldBe` Nothing
      describe "findRoad" $ do
        it "detects a white road through mixed paths" $ do
          let board =
                fromLists
                  [ [ [Piece White Flat]
                    , []
                    , [Piece White Flat]
                    , [Piece White Flat]
                    ]
                  , [[], [Piece White Flat], [], [Piece White Flat]]
                  , [[Piece White Flat], [], [Piece White Flat], []]
                  , [[], [Piece White Flat], [], [Piece White Flat]]
                  ]
          checkGameWin board `shouldBe` Just (Win White)
        it "detects a black road through mixed paths" $ do
          let board =
                fromLists
                  [ [ [Piece Black Flat]
                    , []
                    , [Piece Black Flat]
                    , [Piece Black Flat]
                    ]
                  , [[], [Piece Black Flat], [], [Piece Black Flat]]
                  , [[Piece Black Flat], [], [Piece Black Flat], []]
                  , [[], [Piece Black Flat], [], [Piece Black Flat]]
                  ]
          checkGameWin board `shouldBe` Just (Win Black)
        it "prevents road through standing stones" $ do
          let board =
                fromLists
                  [ [ [Piece White Flat]
                    , [Piece White Standing]
                    , [Piece White Flat]
                    , [Piece White Flat]
                    ]
                  , [[], [], [], []]
                  , [[], [], [], []]
                  , [[], [], [], []]
                  ]
          findRoad board White (Position 1 1) `shouldBe` False
