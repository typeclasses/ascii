module Main (main) where

import Test.Hspec

import ASCII.Refinement (ASCII, asciiUnsafe)

import qualified ASCII.Lift as Lift
import qualified ASCII.Refinement as Refinement

import ASCII.Char (Char (..))

import Data.Text (Text)
import Data.Word (Word8)
import Prelude

main :: IO ()
main = hspec do

    describe "lift" do

        it "letter" do
            let f x = Lift.lift x :: Word8
            f CapitalLetterA `shouldBe` 65

        it "list" do
            let f x = Lift.lift x :: Text
            f [CapitalLetterH,SmallLetterI,ExclamationMark] `shouldBe` "Hi!"

    describe "refinement" do

        it "validateChar" do
            let f x = Refinement.validateChar x :: Maybe (ASCII Int)
            f (-1) `shouldBe` Nothing
            f 65 `shouldBe` Just (asciiUnsafe 65)
            f 97 `shouldBe` Just (asciiUnsafe 97)
            f 128 `shouldBe` Nothing

        it "fromCharList" do
            let f x = Refinement.fromCharList x :: ASCII Text
            f [CapitalLetterH,SmallLetterI,ExclamationMark] `shouldBe` asciiUnsafe "Hi!"

        it "toCharList" do
            let f x = Refinement.toCharList
                          (Refinement.substituteString x :: ASCII Text)
            f "Piñata" `shouldBe` [CapitalLetterP, SmallLetterI, Substitute,
                                   SmallLetterA, SmallLetterT, SmallLetterA]

        it "substituteString" do
            let f x = Refinement.substituteString x :: ASCII Text
            f "Cristóbal" `shouldBe` asciiUnsafe "Crist\SUBbal"

        it "validateString" do
            let f x = Refinement.validateString x :: Maybe (ASCII Text)
            f "Hello" `shouldBe` Just (asciiUnsafe "Hello")
            f "Cristóbal" `shouldBe` Nothing
