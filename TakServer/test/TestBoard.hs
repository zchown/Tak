module TestBoard where

import Board
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as T
import qualified TPS
import Test.Hspec

runBoardTests :: IO ()
runBoardTests =
  hspec $ do
    describe "Board Module" $ do
      describe "createEmptyBoard" $ do
        it "creates a 4x4 board" $ do
          let b = createEmptyBoard 4
          nrows b `shouldBe` 4
          ncols b `shouldBe` 4
          all null (toList b) `shouldBe` True
      describe "getTopPiece" $ do
        it "returns Nothing for an empty square" $ do
          let b = createEmptyBoard 4
          getTopPiece 1 1 b `shouldBe` Nothing
        it "returns the top piece of a non-empty stack" $ do
          let b = setElem [Piece White Flat] (1, 1) (createEmptyBoard 4)
          getTopPiece 1 1 b `shouldBe` Just (Piece White Flat)
      describe "addToStack" $ do
        it "adds a piece to an empty stack" $ do
          addToStack [] (Piece White Flat) `shouldBe` Just [Piece White Flat]
        it "prevents adding to a non-flat top piece" $ do
          let stack = [Piece White Standing]
          addToStack stack (Piece Black Flat) `shouldBe` Nothing
      describe "getAllPieces" $ do
        it "returns all pieces of a specific color" $ do
          let b =
                fromLists
                  [ [[Piece White Flat], [Piece Black Standing]]
                  , [[Piece White Cap], [Piece Black Flat]]
                  ]
          getAllPieces b White `shouldBe` [Piece White Flat, Piece White Cap]
      describe "getPlaced" $ do
        it "correctly counts placed pieces" $ do
          let b =
                fromLists
                  [ [[Piece White Flat], [Piece Black Standing]]
                  , [[Piece White Cap], [Piece Black Flat]]
                  ]
          getPlaced b White `shouldBe` Reserves {stones = 1, caps = 1}
      describe "letterToCol and colToLetter" $ do
        it "converts between letters and column numbers" $ do
          letterToCol 'a' `shouldBe` 1
          letterToCol 'b' `shouldBe` 2
          colToLetter 1 `shouldBe` 'a'
          colToLetter 2 `shouldBe` 'b'
      describe "checkGameResult" $ do
        it "detects non full board" $ do
          let partialBoard = matrix 4 4 (const [])
          checkFullBoard partialBoard False `shouldBe` Continue
        it "detects full board" $ do
          let partialBoard = matrix 4 4 (const [Piece White Flat])
          checkFullBoard partialBoard False `shouldBe` FlatWin White
        it "detects when game is not over" $ do
          let partialBoard = matrix 4 4 (const [])
              gameState =
                GameState
                  { board = partialBoard
                  , turn = White
                  , moveNumber = 0
                  , player1 = Reserves 10 1
                  , player2 = Reserves 10 1
                  , result = Continue
                  , gameHistory = []
                  }
          checkGameResult gameState `shouldBe` Continue
        it "check f1 bug?" $ do
          let b = TPS.parseTPSHard $ T.pack "x6/x6/x6/x6/x6/x5,1 2 2"
          checkGameResult b `shouldBe` Continue
      describe "findRoad" $ do
        it "detects a white road through mixed paths" $ do
          let b =
                board $
                TPS.parseTPSHard $ T.pack "x6/x6/x6/x6/x6/1,1,1,1,1,1 2 2"
          checkGameWin b `shouldBe` Road White
        it "off by 1 bug?" $ do
          let b =
                TPS.parseTPSHard $
                T.pack
                  "1,x3,2C,x/1,x2,2,x2/1,1,1,2,2S,x/1,x,1,x3/2,x,1,x3/x,x,2,x3 2 1"
          checkGameResult b `shouldBe` Continue
        it "detects a black road through mixed paths" $ do
          let b =
                board $
                TPS.parseTPSHard $ T.pack "2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2"
          checkGameWin b `shouldBe` Road Black
        it "prevents road through standing stones" $ do
          let b =
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
          findRoad b White (Position (1, 1)) `shouldBe` False
      it "detects road win" $ do
        let gameState =
              GameState
                { board =
                    board $
                    TPS.parseTPSHard $
                    T.pack "x6/x6/x6/x,212121,x4/22,12,2,2,2,12/x6 1 31"
                , turn = White
                , moveNumber = 0
                , player1 = Reserves 10 1
                , player2 = Reserves 10 1
                , result = Continue
                , gameHistory = []
                }
        checkGameResult gameState `shouldBe` Road Black
      it "uses cap for road" $ do
        let gameState =
              GameState
                { board =
                    board $
                    TPS.parseTPSHard $
                    T.pack
                      "2,x4,1/1,1,1,1,1,x/1,1,12,2,x2/2,2,2C,2,2,2/x6/1,x5 1 21"
                , turn = White
                , moveNumber = 0
                , player1 = Reserves 10 1
                , player2 = Reserves 10 1
                , result = Continue
                , gameHistory = []
                }
        checkGameResult gameState `shouldBe` Road Black
      it "detects reserve game end" $ do
        let gameState =
              GameState
                { board = createEmptyBoard 6
                , turn = White
                , moveNumber = 0
                , player1 = Reserves 0 0
                , player2 = Reserves 10 1
                , result = Continue
                , gameHistory = []
                }
        checkGameResult gameState `shouldBe` Draw
      it "detects flat win" $ do
        let gameState =
              GameState
                { board = placeFlat (createEmptyBoard 6) (Position (1, 1)) White
                , turn = White
                , moveNumber = 0
                , player1 = Reserves 0 0
                , player2 = Reserves 10 1
                , result = Continue
                , gameHistory = []
                }
        checkGameResult gameState `shouldBe` FlatWin White
