{-# LANGUAGE OverloadedStrings #-}

module TestPTN where

import Board as B
import Control.Monad (when)
import Data.Either (fromRight, isRight)
import Data.Text (pack)
import PTN
import Test.Hspec

runPTNTests :: IO ()
runPTNTests =
  hspec $ do
    describe "PTN Parsing" $ do
      describe "parseMove" $ do
        it "parses a flat placement for White" $ do
          parseMove "a1" `shouldBe`
            Prelude.Right (PlaceFlat (Position 1 1, White))
        it "parses a standing placement for White" $ do
          parseMove "!a1" `shouldBe`
            Prelude.Right (PlaceStanding (Position 1 1, White))
        it "parses a capstone placement for White" $ do
          parseMove "+a1" `shouldBe`
            Prelude.Right (PlaceCap (Position 1 1, White))
        it "parses a basic slide move" $ do
          parseMove "a1>" `shouldBe`
            Prelude.Right (Slide (Position 1 1, 1, B.Right, [], White, False))
        it "parses a slide move with multiple drops" $ do
          parseMove "a1>12" `shouldBe`
            Prelude.Right (Slide (Position 1 1, 1, B.Right, [2], White, False))
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
               \a1\n!b2\n+c3\na1>"
          let result = parsePTN (pack ptnText)
          result `shouldSatisfy` isRight
          when (isRight result) $ do
            let Prelude.Right ptn = result
            site ptn `shouldBe` "Test"
            event ptn `shouldBe` "Test Event"
            p1 ptn `shouldBe` "Alice"
            p2 ptn `shouldBe` "Bob"
            moves ptn `shouldBe`
              [ PlaceFlat (Position 1 1, White)
              , PlaceStanding (Position 2 2, White)
              , PlaceCap (Position 3 3, White)
              , Slide (Position 1 1, 1, B.Right, [], White, False)
              ]
