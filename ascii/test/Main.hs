module Main (main) where

import ASCII

import ASCII.Char (Char (..))
import ASCII.Refinement (asciiUnsafe)

import Data.Bool (Bool (..))
import Data.Function (($))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Word (Word8)
import Prelude (enumFromTo, maxBound, minBound)
import System.IO (IO)

import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

    describe "charGroup" $ do
        let c ~> g = charGroup c `shouldBe` g

        it "A is printable"                 $ CapitalLetterA    ~> Printable
        it "end-of-transmission is control" $ EndOfTransmission ~> Control

    describe "inGroup" $ do

        describe "tests Char" $ do
            it "end-of-transmission is not printable" $
                inGroup Printable EndOfTransmission `shouldBe` False
            it "end-of-transmission is control" $
                inGroup Control   EndOfTransmission `shouldBe` True

        describe "tests Int" $ do
            let i ~> b = inGroup Printable (i :: Int) `shouldBe` b

            it "-1 is not printable (isn't anything)"  $ (-1) ~> False
            it "5 is not printable"                    $ 5    ~> False
            it "65 is printable"                       $ 65   ~> True
            it "97 is printable"                       $ 97   ~> True
            it "127 is not printable"                  $ 127  ~> False
            it "128 is not printable (isn't anything)" $ 128  ~> False

    describe "letterCase" $ do
        let ch ~> ca = letterCase ch `shouldBe` ca

        describe "works with Char" $ do
            it "a is lower"    $ SmallLetterA    ~> Just LowerCase
            it "A is upper"    $ CapitalLetterA  ~> Just UpperCase
            it "! has no case" $ ExclamationMark ~> Nothing

        it "works with ASCII Word8" $ do
            map letterCase ([string|Hey!|] :: [ASCII Word8]) `shouldBe`
                [Just UpperCase, Just LowerCase, Just LowerCase, Nothing]

    describe "isCase" $ do

        describe "works with Char" $ do
            let c ~> b = isCase UpperCase c `shouldBe` b

            it "a is not upper" $ SmallLetterA    ~> False
            it "A is upper"     $ CapitalLetterA  ~> True
            it "! is not upper" $ ExclamationMark ~> False

        it "works with ASCII Word8" $ do
            map (isCase UpperCase) ([string|Hey!|] :: [ASCII Word8])
                `shouldBe` [True, False, False, False]

        describe "works with Int" $ do
            let i ~> b = isCase UpperCase (i :: Int) `shouldBe` b

            it "-1 is not upper (isn't anything)"  $ (-1) ~> False
            it "65 is upper"                       $ 65   ~> True
            it "97 is not upper"                   $ 97   ~> False
            it "128 is not upper (isn't anything)" $ 128  ~> False

    describe "toCaseChar" $ do

        describe "works with Char" $ do
            it "a to upper is A" $ toCaseChar UpperCase SmallLetterA
                `shouldBe` CapitalLetterA

        describe "works with ASCII Word8" $ do
            it "a to upper is 65" $ toCaseChar UpperCase ([char|a|] :: ASCII Word8)
                `shouldBe` (asciiUnsafe 65 :: ASCII Word8)

    describe "toCaseString" $ do
        it "converts each Char to a case" $ do
            let check a b = toCaseString UpperCase a `shouldBe` b
            check [ CapitalLetterH, SmallLetterE,   SmallLetterY,   ExclamationMark ]
                  [ CapitalLetterH, CapitalLetterE, CapitalLetterY, ExclamationMark ]

        it "works with ASCII Text" $
            (toCaseString UpperCase [string|Hey!|] :: ASCII Text)
                `shouldBe` asciiUnsafe "HEY!"

    describe "charToInt" $ do
        let c ~> i = charToInt c `shouldBe` i

        it "Null is 0"     $ Null           ~> 0
        it "A is 65"       $ CapitalLetterA ~> 65
        it "a is 97"       $ SmallLetterA   ~> 97
        it "Delete is 127" $ Delete         ~> 127

    describe "charToWord8" $ do
        let c ~> i = charToWord8 c `shouldBe` i

        it "Null is 0"     $ Null           ~> 0
        it "A is 65"       $ CapitalLetterA ~> 65
        it "a is 97"       $ SmallLetterA   ~> 97
        it "Delete is 127" $ Delete         ~> 127

    describe "charListToText" $ do
        it "packs a list of Char into Text" $
            charListToText [CapitalLetterH, SmallLetterI, ExclamationMark]
                `shouldBe` "Hi!"

    describe "fromChar" $ do
        it "converts Char to Word8" $ (fromChar CapitalLetterA :: Word8) `shouldBe` 65

    describe "fromCharList" $ do
        it "converts [Char] to Text" $
            (fromCharList [CapitalLetterH, SmallLetterI, ExclamationMark] :: Text)
                `shouldBe` "Hi!"

    describe "byteListToUnicodeStringMaybe" $ do
        it "converts [Word8] to String" $
            ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x50]
                `shouldBe` Just "HTTP"
        it "can fail" $ ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x80]
            `shouldBe` Nothing

    describe "digitString" $ do
        it "converts Digit to a single character of Text" $ do
            let f = digitString :: Digit -> Text
            map f (enumFromTo minBound maxBound) `shouldBe`
                ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

    describe "hexCharString" $ do
        it "converts HexChar to a single character of Text" $ do
            let f = hexCharString :: HexChar -> Text
            map f (enumFromTo minBound maxBound) `shouldBe`
                [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
                , "A", "B", "C", "D", "E", "F"
                , "a", "b", "c", "d", "e", "f" ]
