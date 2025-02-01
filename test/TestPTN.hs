{-# LANGUAGE OverloadedStrings #-}

module TestPTN where

import Board as B
import Data.Either (isLeft)
import Data.Text (pack)
import PTN
import Test.Hspec

runPTNTests :: IO ()
runPTNTests =
  hspec $ do
    describe "PTN Parsing" $ do
      describe "parseSingleMove" $ do
        it "parses a flat placement for White" $ do
          parseSingleMove "a1" B.White `shouldBe`
            Prelude.Right (B.PlaceFlat (B.Position (1, 1), B.White))
        it "parses a standing placement for White" $ do
          parseSingleMove "Sa1" B.White `shouldBe`
            Prelude.Right (B.PlaceStanding (B.Position (1, 1), B.White))
        it "parses a capstone placement for White" $ do
          parseSingleMove "Ca1" B.White `shouldBe`
            Prelude.Right (B.PlaceCap (B.Position (1, 1), B.White))
        it "parses a basic slide move for White" $ do
          parseSingleMove "a1>" B.White `shouldBe`
            Prelude.Right
              (B.Slide (B.Position (1, 1), 1, B.Right, [1], B.White, False))
        it "parses a slide move with multiple drops for White" $ do
          parseSingleMove "3a1>12" B.White `shouldBe`
            Prelude.Right
              (B.Slide (B.Position (1, 1), 3, B.Right, [1, 2], B.White, False))
        it "fails to parse an invalid move format" $ do
          isLeft (parseSingleMove "invalid" B.White) `shouldBe` True
      describe "parseMovePair" $ do
        it "parses a move pair with White and Black moves" $ do
          parseMovePair "1. a1 b1" `shouldBe`
            Prelude.Right
              [ B.PlaceFlat (B.Position (1, 1), B.White)
              , B.PlaceFlat (B.Position (2, 1), B.Black)
              ]
        it "parses a move pair with only White's move (partial move)" $ do
          parseMovePair "1. a1" `shouldBe`
            Prelude.Right [B.PlaceFlat (B.Position (1, 1), B.White)]
        it "fails to parse a move pair with an invalid format" $ do
          isLeft (parseMovePair "invalid") `shouldBe` True
        it "fails to parse a move pair with an invalid White move" $ do
          isLeft (parseMovePair "1. invalid b1") `shouldBe` True
        it "fails to parse a move pair with an invalid Black move" $ do
          isLeft (parseMovePair "1. a1 invalid") `shouldBe` True
      describe "parsePTN" $ do
        it "parses a full PTN string with metadata and moves" $ do
          let ptnText =
                "[Site: Test]\n\
               \[Event: Test Event]\n\
               \[Date: 2024-01-26]\n\
               \[Time: 10:00]\n\
               \[Player1: Alice]\n\
               \[Player2: Bob]\n\
               \[Clock: 30m]\n\
               \[Result: 1-0]\n\
               \[Size: 6]\n\
               \1. a1 b1\n\
               \2. c1 d1\n"
          let result' = parsePTN (pack ptnText)
          case result' of
            Prelude.Right ptn -> do
              site ptn `shouldBe` Just "Test"
              event ptn `shouldBe` Just "Test Event"
              p1 ptn `shouldBe` Just "Alice"
              p2 ptn `shouldBe` Just "Bob"
              size ptn `shouldBe` Just 6
              moves ptn `shouldBe`
                [ B.PlaceFlat (B.Position (1, 1), B.White)
                , B.PlaceFlat (B.Position (2, 1), B.Black)
                , B.PlaceFlat (B.Position (3, 1), B.White)
                , B.PlaceFlat (B.Position (4, 1), B.Black)
                ]
            Prelude.Left e -> error (show e)
        it "parses a PTN string with a partial move (only White's move)" $ do
          let ptnText =
                "[Site: Test]\n\
               \[Event: Test Event]\n\
               \[Date: 2024-01-26]\n\
               \[Time: 10:00]\n\
               \[Player1: Alice]\n\
               \[Player2: Bob]\n\
               \[Clock: 30m]\n\
               \[Result: 1-0]\n\
               \[Size: 6]\n\
               \1. a1\n"
          let result' = parsePTN (pack ptnText)
          case result' of
            Prelude.Right ptn -> do
              moves ptn `shouldBe` [B.PlaceFlat (B.Position (1, 1), B.White)]
            Prelude.Left e -> error (show e)
        it "parses a PTN string with missing metadata" $ do
          let ptnText =
                "[Site: Test]\n\
               \[Event: Test Event]\n\
               \1. a1 b1\n"
          let result' = parsePTN (pack ptnText)
          case result' of
            Prelude.Right ptn -> do
              site ptn `shouldBe` Just "Test"
              event ptn `shouldBe` Just "Test Event"
              date ptn `shouldBe` Nothing
              time ptn `shouldBe` Nothing
              p1 ptn `shouldBe` Nothing
              p2 ptn `shouldBe` Nothing
              clock ptn `shouldBe` Nothing
              ptnResult ptn `shouldBe` Nothing
              size ptn `shouldBe` Nothing
              moves ptn `shouldBe`
                [ B.PlaceFlat (B.Position (1, 1), B.White)
                , B.PlaceFlat (B.Position (2, 1), B.Black)
                ]
            Prelude.Left e -> error (show e)
        it "fails to parse a PTN string with invalid moves" $ do
          let ptnText =
                "[Site: Test]\n\
               \[Event: Test Event]\n\
               \[Date: 2024-01-26]\n\
               \[Time: 10:00]\n\
               \[Player1: Alice]\n\
               \[Player2: Bob]\n\
               \[Clock: 30m]\n\
               \[Result: 1-0]\n\
               \[Size: 6]\n\
               \1. invalid b1\n"
          isLeft (parsePTN (pack ptnText)) `shouldBe` True
