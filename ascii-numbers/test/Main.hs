module Main (main) where

import ASCII.Case (Case (..))
import qualified ASCII.Char as ASCII
import qualified ASCII.Decimal as Dec
import qualified ASCII.Hexadecimal as Hex
import qualified ASCII.Word4 as Word4

import Control.Applicative (liftA2)
import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Eq (Eq)
import Data.Foldable (for_)
import Data.Function (fix, ($), (.))
import Data.List (map, splitAt)
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Tuple (uncurry)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Prelude (Bounded (..), Enum (..), Integer, negate, (+), (-))
import System.Exit (exitFailure)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Invert as I

import Hedgehog (Gen, MonadTest, Property, PropertyT, checkParallel, discover,
                 forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

prop_showNatural_12 :: Property
prop_showNatural_12 = withTests 1 $ property $
    Dec.showNatural 12 === ("12" :: T.Text)

prop_showNatural_268 :: Property
prop_showNatural_268 = withTests 1 $ property $
    Dec.showNatural 268 === ("268" :: String)

prop_showInteger_12 :: Property
prop_showInteger_12 = withTests 1 $ property $
    Dec.showInteger 12 === ("12" :: T.Text)

prop_showInteger_minus12 :: Property
prop_showInteger_minus12 = withTests 1 $ property $
    Dec.showInteger (negate 12) === ("-12" :: LT.Text)

prop_showInteger_268 :: Property
prop_showInteger_268 = withTests 1 $ property $
    Dec.showInteger 268 === ("268" :: LT.Text)

prop_showInteger_minus268 :: Property
prop_showInteger_minus268 = withTests 1 $ property $
    Dec.showInteger (negate 268) === ("-268" :: LT.Text)

prop_showNatural_0_to_9 :: Property
prop_showNatural_0_to_9 = withTests 1 $ property $
    T.unwords (map Dec.showNatural [0..9]) === ("0 1 2 3 4 5 6 7 8 9" :: T.Text)

prop_showNatural_10_to_99 :: Property
prop_showNatural_10_to_99 = withTests 1 $ property $
    let
        ne f xs = case xs of [] -> []; _ -> f xs
        groupsOf n = fix $ \r -> ne $ \xs -> let (x, ys) = splitAt n xs in x : r ys
        res :: [T.Text]
        res = map T.unwords $ groupsOf 10 $ map Dec.showNatural [10..99]
    in
        res ===
          [ "10 11 12 13 14 15 16 17 18 19"
          , "20 21 22 23 24 25 26 27 28 29"
          , "30 31 32 33 34 35 36 37 38 39"
          , "40 41 42 43 44 45 46 47 48 49"
          , "50 51 52 53 54 55 56 57 58 59"
          , "60 61 62 63 64 65 66 67 68 69"
          , "70 71 72 73 74 75 76 77 78 79"
          , "80 81 82 83 84 85 86 87 88 89"
          , "90 91 92 93 94 95 96 97 98 99"
          ]

prop_showNaturalHex_0_to_f_lower :: Property
prop_showNaturalHex_0_to_f_lower = withTests 1 $ property $
    T.unwords (List.map (Hex.showNatural LowerCase) [0..15])
        === "0 1 2 3 4 5 6 7 8 9 a b c d e f"

prop_showNaturalHex_0_to_f_upper :: Property
prop_showNaturalHex_0_to_f_upper = withTests 1 $ property $
    T.unwords (List.map (Hex.showNatural UpperCase) [0..15])
        === "0 1 2 3 4 5 6 7 8 9 A B C D E F"

prop_showNaturalHex_10_to_ff :: Property
prop_showNaturalHex_10_to_ff = withTests 1 $ property $
    let
        ne f xs = case xs of [] -> []; _ -> f xs
        groupsOf n = fix $ \r -> ne $ \xs -> let (x, ys) = splitAt n xs in x : r ys
        res :: [LT.Text]
        res = map LT.unwords $ groupsOf 16 $ map (Hex.showNatural LowerCase) [16..255]
    in
        res ===
          [ "10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f"
          , "20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f"
          , "30 31 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f"
          , "40 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f"
          , "50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f"
          , "60 61 62 63 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f"
          , "70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f"
          , "80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f"
          , "90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f"
          , "a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af"
          , "b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf"
          , "c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf"
          , "d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df"
          , "e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 ea eb ec ed ee ef"
          , "f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fa fb fc fd fe ff"
          ]

prop_toDigitListMaybe :: Property
prop_toDigitListMaybe = withTests 1 $ property $
    Dec.toDigitListMaybe ("846" :: T.Text) === Just [Dec.Digit8, Dec.Digit4, Dec.Digit6]

prop_toDigitListMaybe_fail :: Property
prop_toDigitListMaybe_fail = withTests 1 $ property $
    Dec.toDigitListMaybe ("84a6" :: T.Text) === Nothing

prop_showNatural_0 :: Property
prop_showNatural_0 = withTests 1 $ property $
    Dec.showNatural 0 === ("0" :: T.Text)

prop_showInteger_0 :: Property
prop_showInteger_0 = withTests 1 $ property $
    Dec.showInteger 0 === ("0" :: T.Text)

prop_readNatural_0 :: Property
prop_readNatural_0 = withTests 1 $ property $
    Dec.readNatural ("0" :: T.Text) === Just 0

prop_readNatural_268 :: Property
prop_readNatural_268 = withTests 1 $ property $
    Dec.readNatural ("268" :: BS.ByteString) === Just 268

prop_readNatural_0004 :: Property
prop_readNatural_0004 = withTests 1 $ property $
    Dec.readNatural ("0004" :: T.Text) === Just 4

prop_readNatural_empty :: Property
prop_readNatural_empty = withTests 1 $ property $
    Dec.readNatural ("" :: T.Text) === Nothing

prop_readNatural_minus4 :: Property
prop_readNatural_minus4 = withTests 1 $ property $
    Dec.readNatural ("-4" :: BS.ByteString) === Nothing

prop_readNatural_big :: Property
prop_readNatural_big = withTests 1 $ property $
    Dec.readNatural ("12345678901234567890" :: T.Text) === Just 12345678901234567890

prop_readInteger_0 :: Property
prop_readInteger_0 = withTests 1 $ property $
    Dec.readInteger ("0" :: T.Text) === Just 0

prop_readInteger_268 :: Property
prop_readInteger_268 = withTests 1 $ property $
    Dec.readInteger ("268" :: T.Text) === Just 268

prop_readInteger_0004 :: Property
prop_readInteger_0004 = withTests 1 $ property $
    Dec.readInteger ("0004" :: T.Text) === Just 4

prop_readInteger_empty :: Property
prop_readInteger_empty = withTests 1 $ property $
    Dec.readInteger ("" :: T.Text) === Nothing

prop_readInteger_minus4 :: Property
prop_readInteger_minus4 = withTests 1 $ property $
    Dec.readInteger ("-4" :: BS.ByteString) === Just (-4)

prop_readInteger_minus :: Property
prop_readInteger_minus = withTests 1 $ property $
    Dec.readInteger ("-" :: T.Text) === Nothing

prop_readInteger_big :: Property
prop_readInteger_big = withTests 1 $ property $
    Dec.readInteger ("12345678901234567890" :: BS.ByteString) === Just 12345678901234567890

prop_readIntegral_word8_0 :: Property
prop_readIntegral_word8_0 = withTests 1 $ property $
    Dec.readIntegral ("0" :: T.Text) === Just (0 :: Word8)

prop_readIntegral_word8_175 :: Property
prop_readIntegral_word8_175 = withTests 1 $ property $
    Dec.readIntegral ("175" :: T.Text) === Just (175 :: Word8)

prop_readIntegral_word8_268 :: Property
prop_readIntegral_word8_268 = withTests 1 $ property $
    Dec.readIntegral ("268" :: T.Text) === (Nothing :: Maybe Word8)

prop_readIntegral_word8_0004 :: Property
prop_readIntegral_word8_0004 = withTests 1 $ property $
    Dec.readIntegral ("0004" :: T.Text) === Just (4 :: Word8)

prop_readIntegral_word8_empty :: Property
prop_readIntegral_word8_empty = withTests 1 $ property $
    Dec.readIntegral ("" :: T.Text) === (Nothing :: Maybe Word8)

prop_readIntegral_word8_minus4 :: Property
prop_readIntegral_word8_minus4 = withTests 1 $ property $
    Dec.readIntegral ("-4" :: T.Text) === (Nothing :: Maybe Word8)

prop_readIntegral_word8_big :: Property
prop_readIntegral_word8_big = withTests 1 $ property $
    Dec.readIntegral ("12345678901234567890" :: T.Text) === (Nothing :: Maybe Word8)

prop_readIntegral_natural_4 :: Property
prop_readIntegral_natural_4 = withTests 1 $ property $
    Dec.readIntegral ("4" :: T.Text) === Just (4 :: Natural)

prop_readIntegral_natural_minus4 :: Property
prop_readIntegral_natural_minus4 = withTests 1 $ property $
    Dec.readIntegral ("-4" :: T.Text) === (Nothing :: Maybe Natural)

prop_naturalDigitMaybe_5 :: Property
prop_naturalDigitMaybe_5 = withTests 1 $ property $
    Dec.naturalDigitMaybe 5 === Just Dec.Digit5

prop_naturalDigitMaybe_12 :: Property
prop_naturalDigitMaybe_12 = withTests 1 $ property $
    Dec.naturalDigitMaybe 12 === Nothing

prop_integerDigitMaybe_5 :: Property
prop_integerDigitMaybe_5 = withTests 1 $ property $
    Dec.integerDigitMaybe 5 === Just Dec.Digit5

prop_integerDigitMaybe_12 :: Property
prop_integerDigitMaybe_12 = withTests 1 $ property $
    Dec.integerDigitMaybe 12 === Nothing

checkEnumBounded :: forall a f. (MonadTest f, Enum a, Bounded a, Eq a, Show a) => [a] -> f ()
checkEnumBounded xs =
  do
    [minBound .. maxBound] === xs
    List.map toEnum [0 .. List.length xs - 1] === xs
    List.map fromEnum xs === [0 .. List.length xs - 1]

prop_D16_enumBounded :: Property
prop_D16_enumBounded = withTests 1 $ property $
    checkEnumBounded
      [ Word4.Number0, Word4.Number1, Word4.Number2, Word4.Number3
      , Word4.Number4, Word4.Number5, Word4.Number6, Word4.Number7
      , Word4.Number8, Word4.Number9, Word4.Number10, Word4.Number11
      , Word4.Number12, Word4.Number13, Word4.Number14, Word4.Number15
      ]

prop_HexLetter_enumBounded :: Property
prop_HexLetter_enumBounded = withTests 1 $ property $
    checkEnumBounded
      [ Hex.LetterA, Hex.LetterB, Hex.LetterC
      , Hex.LetterD, Hex.LetterE, Hex.LetterF
      ]

prop_HexChar_enumBounded :: Property
prop_HexChar_enumBounded = withTests 1 $ property $
    checkEnumBounded
      [ Hex.Digit0, Hex.Digit1, Hex.Digit2, Hex.Digit3, Hex.Digit4
      , Hex.Digit5, Hex.Digit6, Hex.Digit7, Hex.Digit8, Hex.Digit9
      , Hex.CapitalLetterA, Hex.CapitalLetterB, Hex.CapitalLetterC
      , Hex.CapitalLetterD, Hex.CapitalLetterE, Hex.CapitalLetterF
      , Hex.SmallLetterA, Hex.SmallLetterB, Hex.SmallLetterC
      , Hex.SmallLetterD, Hex.SmallLetterE, Hex.SmallLetterF
      ]

prop_HexCharBreakdown_enumBounded :: Property
prop_HexCharBreakdown_enumBounded = withTests 1 $ property $
    checkEnumBounded
      [ Hex.HexDigit Dec.Digit0
      , Hex.HexDigit Dec.Digit1
      , Hex.HexDigit Dec.Digit2
      , Hex.HexDigit Dec.Digit3
      , Hex.HexDigit Dec.Digit4
      , Hex.HexDigit Dec.Digit5
      , Hex.HexDigit Dec.Digit6
      , Hex.HexDigit Dec.Digit7
      , Hex.HexDigit Dec.Digit8
      , Hex.HexDigit Dec.Digit9
      , Hex.HexLetter UpperCase Hex.LetterA
      , Hex.HexLetter UpperCase Hex.LetterB
      , Hex.HexLetter UpperCase Hex.LetterC
      , Hex.HexLetter UpperCase Hex.LetterD
      , Hex.HexLetter UpperCase Hex.LetterE
      , Hex.HexLetter UpperCase Hex.LetterF
      , Hex.HexLetter LowerCase Hex.LetterA
      , Hex.HexLetter LowerCase Hex.LetterB
      , Hex.HexLetter LowerCase Hex.LetterC
      , Hex.HexLetter LowerCase Hex.LetterD
      , Hex.HexLetter LowerCase Hex.LetterE
      , Hex.HexLetter LowerCase Hex.LetterF
      ]

every :: (Enum a, Bounded a) => [a]
every = [minBound .. maxBound]

functionEq :: (MonadTest f, Eq a, Show a) => [t] -> (t -> a) -> (t -> a) -> f ()
functionEq xs f g =
    for_ xs $ \x ->
        f x === g x

prop_hexLetterD16 :: Property
prop_hexLetterD16 = withTests 1 $ property $
    functionEq every Hex.hexLetterD16 $ \case
        Hex.LetterA -> Word4.Number10
        Hex.LetterB -> Word4.Number11
        Hex.LetterC -> Word4.Number12
        Hex.LetterD -> Word4.Number13
        Hex.LetterE -> Word4.Number14
        Hex.LetterF -> Word4.Number15

prop_d16HexLetter :: Property
prop_d16HexLetter = withTests 1 $ property $
    functionEq every Hex.d16HexLetter $
        I.injection I.linearSearchLazy every Hex.hexLetterD16

prop_letterHexChar :: Property
prop_letterHexChar = withTests 1 $ property $
    functionEq (liftA2 (,) every every) (uncurry Hex.letterHexChar) $ \case
        (UpperCase, Hex.LetterA) -> Hex.CapitalLetterA
        (UpperCase, Hex.LetterB) -> Hex.CapitalLetterB
        (UpperCase, Hex.LetterC) -> Hex.CapitalLetterC
        (UpperCase, Hex.LetterD) -> Hex.CapitalLetterD
        (UpperCase, Hex.LetterE) -> Hex.CapitalLetterE
        (UpperCase, Hex.LetterF) -> Hex.CapitalLetterF
        (LowerCase, Hex.LetterA) -> Hex.SmallLetterA
        (LowerCase, Hex.LetterB) -> Hex.SmallLetterB
        (LowerCase, Hex.LetterC) -> Hex.SmallLetterC
        (LowerCase, Hex.LetterD) -> Hex.SmallLetterD
        (LowerCase, Hex.LetterE) -> Hex.SmallLetterE
        (LowerCase, Hex.LetterF) -> Hex.SmallLetterF

prop_hexCharLetter :: Property
prop_hexCharLetter = withTests 1 $ property $
    functionEq every Hex.hexCharLetter $ \case
        Hex.CapitalLetterA -> Just Hex.LetterA
        Hex.CapitalLetterB -> Just Hex.LetterB
        Hex.CapitalLetterC -> Just Hex.LetterC
        Hex.CapitalLetterD -> Just Hex.LetterD
        Hex.CapitalLetterE -> Just Hex.LetterE
        Hex.CapitalLetterF -> Just Hex.LetterF
        Hex.SmallLetterA   -> Just Hex.LetterA
        Hex.SmallLetterB   -> Just Hex.LetterB
        Hex.SmallLetterC   -> Just Hex.LetterC
        Hex.SmallLetterD   -> Just Hex.LetterD
        Hex.SmallLetterE   -> Just Hex.LetterE
        Hex.SmallLetterF   -> Just Hex.LetterF
        _                  -> Nothing

prop_hexAsciiChar :: Property
prop_hexAsciiChar = withTests 1 $ property $
    functionEq every Hex.hexAsciiChar $ \case
        Hex.Digit0          ->  ASCII.Digit0
        Hex.Digit1          ->  ASCII.Digit1
        Hex.Digit2          ->  ASCII.Digit2
        Hex.Digit3          ->  ASCII.Digit3
        Hex.Digit4          ->  ASCII.Digit4
        Hex.Digit5          ->  ASCII.Digit5
        Hex.Digit6          ->  ASCII.Digit6
        Hex.Digit7          ->  ASCII.Digit7
        Hex.Digit8          ->  ASCII.Digit8
        Hex.Digit9          ->  ASCII.Digit9
        Hex.CapitalLetterA  ->  ASCII.CapitalLetterA
        Hex.CapitalLetterB  ->  ASCII.CapitalLetterB
        Hex.CapitalLetterC  ->  ASCII.CapitalLetterC
        Hex.CapitalLetterD  ->  ASCII.CapitalLetterD
        Hex.CapitalLetterE  ->  ASCII.CapitalLetterE
        Hex.CapitalLetterF  ->  ASCII.CapitalLetterF
        Hex.SmallLetterA    ->  ASCII.SmallLetterA
        Hex.SmallLetterB    ->  ASCII.SmallLetterB
        Hex.SmallLetterC    ->  ASCII.SmallLetterC
        Hex.SmallLetterD    ->  ASCII.SmallLetterD
        Hex.SmallLetterE    ->  ASCII.SmallLetterE
        Hex.SmallLetterF    ->  ASCII.SmallLetterF

prop_asciiCharHex :: Property
prop_asciiCharHex = withTests 1 $ property $
    functionEq every Hex.asciiCharHex $
        I.injection I.linearSearchLazy every Hex.hexAsciiChar

prop_d16HexChar :: Property
prop_d16HexChar = withTests 1 $ property $
    functionEq (liftA2 (,) every every) (uncurry Hex.d16HexChar) $ \case
        (_, Word4.Number0) -> Hex.Digit0
        (_, Word4.Number1) -> Hex.Digit1
        (_, Word4.Number2) -> Hex.Digit2
        (_, Word4.Number3) -> Hex.Digit3
        (_, Word4.Number4) -> Hex.Digit4
        (_, Word4.Number5) -> Hex.Digit5
        (_, Word4.Number6) -> Hex.Digit6
        (_, Word4.Number7) -> Hex.Digit7
        (_, Word4.Number8) -> Hex.Digit8
        (_, Word4.Number9) -> Hex.Digit9
        (UpperCase, Word4.Number10) -> Hex.CapitalLetterA
        (UpperCase, Word4.Number11) -> Hex.CapitalLetterB
        (UpperCase, Word4.Number12) -> Hex.CapitalLetterC
        (UpperCase, Word4.Number13) -> Hex.CapitalLetterD
        (UpperCase, Word4.Number14) -> Hex.CapitalLetterE
        (UpperCase, Word4.Number15) -> Hex.CapitalLetterF
        (LowerCase, Word4.Number10) -> Hex.SmallLetterA
        (LowerCase, Word4.Number11) -> Hex.SmallLetterB
        (LowerCase, Word4.Number12) -> Hex.SmallLetterC
        (LowerCase, Word4.Number13) -> Hex.SmallLetterD
        (LowerCase, Word4.Number14) -> Hex.SmallLetterE
        (LowerCase, Word4.Number15) -> Hex.SmallLetterF

prop_hexCharD16 :: Property
prop_hexCharD16 = withTests 1 $ property $
    functionEq every Hex.hexCharD16 $ \case
        Hex.Digit0 -> Word4.Number0
        Hex.Digit1 -> Word4.Number1
        Hex.Digit2 -> Word4.Number2
        Hex.Digit3 -> Word4.Number3
        Hex.Digit4 -> Word4.Number4
        Hex.Digit5 -> Word4.Number5
        Hex.Digit6 -> Word4.Number6
        Hex.Digit7 -> Word4.Number7
        Hex.Digit8 -> Word4.Number8
        Hex.Digit9 -> Word4.Number9
        Hex.CapitalLetterA -> Word4.Number10
        Hex.CapitalLetterB -> Word4.Number11
        Hex.CapitalLetterC -> Word4.Number12
        Hex.CapitalLetterD -> Word4.Number13
        Hex.CapitalLetterE -> Word4.Number14
        Hex.CapitalLetterF -> Word4.Number15
        Hex.SmallLetterA   -> Word4.Number10
        Hex.SmallLetterB   -> Word4.Number11
        Hex.SmallLetterC   -> Word4.Number12
        Hex.SmallLetterD   -> Word4.Number13
        Hex.SmallLetterE   -> Word4.Number14
        Hex.SmallLetterF   -> Word4.Number15

prop_breakDownHexChar :: Property
prop_breakDownHexChar = withTests 1 $ property $
    functionEq every Hex.breakDownHexChar $ \case
        Hex.Digit0 -> Hex.HexDigit Dec.Digit0
        Hex.Digit1 -> Hex.HexDigit Dec.Digit1
        Hex.Digit2 -> Hex.HexDigit Dec.Digit2
        Hex.Digit3 -> Hex.HexDigit Dec.Digit3
        Hex.Digit4 -> Hex.HexDigit Dec.Digit4
        Hex.Digit5 -> Hex.HexDigit Dec.Digit5
        Hex.Digit6 -> Hex.HexDigit Dec.Digit6
        Hex.Digit7 -> Hex.HexDigit Dec.Digit7
        Hex.Digit8 -> Hex.HexDigit Dec.Digit8
        Hex.Digit9 -> Hex.HexDigit Dec.Digit9
        Hex.CapitalLetterA -> Hex.HexLetter UpperCase Hex.LetterA
        Hex.CapitalLetterB -> Hex.HexLetter UpperCase Hex.LetterB
        Hex.CapitalLetterC -> Hex.HexLetter UpperCase Hex.LetterC
        Hex.CapitalLetterD -> Hex.HexLetter UpperCase Hex.LetterD
        Hex.CapitalLetterE -> Hex.HexLetter UpperCase Hex.LetterE
        Hex.CapitalLetterF -> Hex.HexLetter UpperCase Hex.LetterF
        Hex.SmallLetterA   -> Hex.HexLetter LowerCase Hex.LetterA
        Hex.SmallLetterB   -> Hex.HexLetter LowerCase Hex.LetterB
        Hex.SmallLetterC   -> Hex.HexLetter LowerCase Hex.LetterC
        Hex.SmallLetterD   -> Hex.HexLetter LowerCase Hex.LetterD
        Hex.SmallLetterE   -> Hex.HexLetter LowerCase Hex.LetterE
        Hex.SmallLetterF   -> Hex.HexLetter LowerCase Hex.LetterF

prop_assembleHexChar :: Property
prop_assembleHexChar = withTests 1 $ property $
    functionEq every Hex.assembleHexChar $
        I.bijection I.linearSearchLazy every Hex.breakDownHexChar

prop_hexCharNatural :: Property
prop_hexCharNatural = withTests 1 $ property $
    functionEq every Hex.hexCharNatural $ \case
        Hex.Digit0         -> 0
        Hex.Digit1         -> 1
        Hex.Digit2         -> 2
        Hex.Digit3         -> 3
        Hex.Digit4         -> 4
        Hex.Digit5         -> 5
        Hex.Digit6         -> 6
        Hex.Digit7         -> 7
        Hex.Digit8         -> 8
        Hex.Digit9         -> 9
        Hex.CapitalLetterA -> 10
        Hex.CapitalLetterB -> 11
        Hex.CapitalLetterC -> 12
        Hex.CapitalLetterD -> 13
        Hex.CapitalLetterE -> 14
        Hex.CapitalLetterF -> 15
        Hex.SmallLetterA   -> 10
        Hex.SmallLetterB   -> 11
        Hex.SmallLetterC   -> 12
        Hex.SmallLetterD   -> 13
        Hex.SmallLetterE   -> 14
        Hex.SmallLetterF   -> 15

prop_naturalHexCharMaybe :: Property
prop_naturalHexCharMaybe = withTests 1 $ property $
    functionEq (liftA2 (,) every [0 .. 20]) (uncurry Hex.naturalHexCharMaybe) $ \case
        (_, 0) -> Just Hex.Digit0
        (_, 1) -> Just Hex.Digit1
        (_, 2) -> Just Hex.Digit2
        (_, 3) -> Just Hex.Digit3
        (_, 4) -> Just Hex.Digit4
        (_, 5) -> Just Hex.Digit5
        (_, 6) -> Just Hex.Digit6
        (_, 7) -> Just Hex.Digit7
        (_, 8) -> Just Hex.Digit8
        (_, 9) -> Just Hex.Digit9
        (UpperCase, 10) -> Just Hex.CapitalLetterA
        (UpperCase, 11) -> Just Hex.CapitalLetterB
        (UpperCase, 12) -> Just Hex.CapitalLetterC
        (UpperCase, 13) -> Just Hex.CapitalLetterD
        (UpperCase, 14) -> Just Hex.CapitalLetterE
        (UpperCase, 15) -> Just Hex.CapitalLetterF
        (LowerCase, 10) -> Just Hex.SmallLetterA
        (LowerCase, 11) -> Just Hex.SmallLetterB
        (LowerCase, 12) -> Just Hex.SmallLetterC
        (LowerCase, 13) -> Just Hex.SmallLetterD
        (LowerCase, 14) -> Just Hex.SmallLetterE
        (LowerCase, 15) -> Just Hex.SmallLetterF
        _ -> Nothing

prop_hexCharInteger :: Property
prop_hexCharInteger = withTests 1 $ property $
    functionEq every Hex.hexCharInteger $ \case
        Hex.Digit0         -> 0
        Hex.Digit1         -> 1
        Hex.Digit2         -> 2
        Hex.Digit3         -> 3
        Hex.Digit4         -> 4
        Hex.Digit5         -> 5
        Hex.Digit6         -> 6
        Hex.Digit7         -> 7
        Hex.Digit8         -> 8
        Hex.Digit9         -> 9
        Hex.CapitalLetterA -> 10
        Hex.CapitalLetterB -> 11
        Hex.CapitalLetterC -> 12
        Hex.CapitalLetterD -> 13
        Hex.CapitalLetterE -> 14
        Hex.CapitalLetterF -> 15
        Hex.SmallLetterA   -> 10
        Hex.SmallLetterB   -> 11
        Hex.SmallLetterC   -> 12
        Hex.SmallLetterD   -> 13
        Hex.SmallLetterE   -> 14
        Hex.SmallLetterF   -> 15

prop_integerHexCharMaybe :: Property
prop_integerHexCharMaybe = withTests 1 $ property $
    functionEq (liftA2 (,) every [-4 .. 20]) (uncurry Hex.integerHexCharMaybe) $ \case
        (_, 0) -> Just Hex.Digit0
        (_, 1) -> Just Hex.Digit1
        (_, 2) -> Just Hex.Digit2
        (_, 3) -> Just Hex.Digit3
        (_, 4) -> Just Hex.Digit4
        (_, 5) -> Just Hex.Digit5
        (_, 6) -> Just Hex.Digit6
        (_, 7) -> Just Hex.Digit7
        (_, 8) -> Just Hex.Digit8
        (_, 9) -> Just Hex.Digit9
        (UpperCase, 10) -> Just Hex.CapitalLetterA
        (UpperCase, 11) -> Just Hex.CapitalLetterB
        (UpperCase, 12) -> Just Hex.CapitalLetterC
        (UpperCase, 13) -> Just Hex.CapitalLetterD
        (UpperCase, 14) -> Just Hex.CapitalLetterE
        (UpperCase, 15) -> Just Hex.CapitalLetterF
        (LowerCase, 10) -> Just Hex.SmallLetterA
        (LowerCase, 11) -> Just Hex.SmallLetterB
        (LowerCase, 12) -> Just Hex.SmallLetterC
        (LowerCase, 13) -> Just Hex.SmallLetterD
        (LowerCase, 14) -> Just Hex.SmallLetterE
        (LowerCase, 15) -> Just Hex.SmallLetterF
        _ -> Nothing

prop_naturalD16Maybe :: Property
prop_naturalD16Maybe = withTests 1 $ property $
    functionEq [0 .. 20] Hex.naturalD16Maybe $
        I.injection I.linearSearchLazy every Hex.d16Natural

prop_integerD16Maybe :: Property
prop_integerD16Maybe = withTests 1 $ property $
    functionEq [-4 .. 20] Hex.integerD16Maybe $
        I.injection I.linearSearchLazy every Hex.d16Integer

prop_naturalD16Unsafe :: Property
prop_naturalD16Unsafe = withTests 1 $ property $
    functionEq [0 .. 15]
        (Just . Hex.naturalD16Unsafe)
        Hex.naturalD16Maybe

prop_integerD16Unsafe :: Property
prop_integerD16Unsafe = withTests 1 $ property $
    functionEq [0 .. 15]
        (Just . Hex.integerD16Unsafe)
        Hex.integerD16Maybe

prop_naturalHexCharUnsafe :: Property
prop_naturalHexCharUnsafe = withTests 1 $ property $
    functionEq (liftA2 (,) every [0 .. 15])
        (Just . uncurry Hex.naturalHexCharUnsafe)
        (uncurry Hex.naturalHexCharMaybe)

prop_integerHexCharUnsafe :: Property
prop_integerHexCharUnsafe = withTests 1 $ property $
    functionEq (liftA2 (,) every [0 .. 15])
        (Just . uncurry Hex.integerHexCharUnsafe)
        (uncurry Hex.integerHexCharMaybe)

prop_hex_showNatural_lower_12 :: Property
prop_hex_showNatural_lower_12 = withTests 1 $ property $
    Hex.showNatural LowerCase 12 === ("c" :: LBS.ByteString)

prop_hex_showNatural_upper_10C :: Property
prop_hex_showNatural_upper_10C = withTests 1 $ property $
    Hex.showNatural UpperCase (256 + 12) === ("10C" :: BS.ByteString)

prop_hex_showNatural_upper_0 :: Property
prop_hex_showNatural_upper_0 = withTests 1 $ property $
    Hex.showNatural UpperCase 0 === ("0" :: LBS.ByteString)

prop_hex_showInteger_lower_12 :: Property
prop_hex_showInteger_lower_12 = withTests 1 $ property $
    Hex.showInteger LowerCase 12 === ("c" :: T.Text)

prop_hex_showInteger_lower_minus12 :: Property
prop_hex_showInteger_lower_minus12 = withTests 1 $ property $
    Hex.showInteger LowerCase (negate 12) === ("-c" :: LT.Text)

prop_hex_showInteger_upper_10C :: Property
prop_hex_showInteger_upper_10C = withTests 1 $ property $
    Hex.showInteger UpperCase (256 + 12) === ("10C" :: String)

prop_hex_showInteger_upper_minus10C :: Property
prop_hex_showInteger_upper_minus10C = withTests 1 $ property $
    Hex.showInteger UpperCase (negate (256 + 12)) === ("-10C" :: BS.ByteString)

prop_hex_showInteger_upper_0 :: Property
prop_hex_showInteger_upper_0 = withTests 1 $ property $
    Hex.showInteger UpperCase 0 === ("0" :: String)

prop_hex_readNatural_5 :: Property
prop_hex_readNatural_5 = withTests 1 $ property $
    Hex.readNatural ("5" :: String) === Just 5

prop_hex_readNatural_minus5 :: Property
prop_hex_readNatural_minus5 = withTests 1 $ property $
    Hex.readNatural ("-5" :: T.Text) === Nothing

prop_hex_readNatural_1f :: Property
prop_hex_readNatural_1f = withTests 1 $ property $
    Hex.readNatural ("1f" :: BS.ByteString) === Just 31

prop_hex_readNatural_1F :: Property
prop_hex_readNatural_1F = withTests 1 $ property $
    Hex.readNatural ("1F" :: LT.Text) === Just 31

prop_hex_readNatural_xa :: Property
prop_hex_readNatural_xa = withTests 1 $ property $
    Hex.readNatural ("xa" :: String) === Nothing

prop_hex_readNatural_empty :: Property
prop_hex_readNatural_empty = withTests 1 $ property $
    Hex.readNatural ("" :: T.Text) === Nothing

prop_hex_readInteger_5 :: Property
prop_hex_readInteger_5 = withTests 1 $ property $
    Hex.readInteger ("5" :: T.Text) === Just 5

prop_hex_readInteger_minus5 :: Property
prop_hex_readInteger_minus5 = withTests 1 $ property $
    Hex.readInteger ("-5" :: T.Text) === Just (-5)

prop_hex_readInteger_1f :: Property
prop_hex_readInteger_1f = withTests 1 $ property $
    Hex.readInteger ("1f" :: LT.Text) === Just 31

prop_hex_readInteger_1F :: Property
prop_hex_readInteger_1F = withTests 1 $ property $
    Hex.readInteger ("1F" :: LT.Text) === Just 31

prop_hex_readInteger_xa :: Property
prop_hex_readInteger_xa = withTests 1 $ property $
    Hex.readInteger ("xa" :: String) === Nothing

prop_hex_readInteger_empty :: Property
prop_hex_readInteger_empty = withTests 1 $ property $
    Hex.readInteger ("" :: String) === Nothing

prop_hex_readInteger_minus :: Property
prop_hex_readInteger_minus = withTests 1 $ property $
    Hex.readInteger ("-" :: String) === Nothing

prop_hex_readIntegral_word8_0014 :: Property
prop_hex_readIntegral_word8_0014 = withTests 1 $ property $
    Hex.readIntegral ("0014" :: String) === Just (20 :: Word8)

prop_hex_readIntegral_word8_empty :: Property
prop_hex_readIntegral_word8_empty = withTests 1 $ property $
    Hex.readIntegral ("" :: String) === (Nothing :: Maybe Word8)

prop_hex_readIntegral_word8_minus4 :: Property
prop_hex_readIntegral_word8_minus4 = withTests 1 $ property $
    Hex.readIntegral ("-4" :: String) === (Nothing :: Maybe Word8)

prop_hex_readIntegral_word8_1234 :: Property
prop_hex_readIntegral_word8_1234 = withTests 1 $ property $
    Hex.readIntegral ("1234" :: String) === (Nothing :: Maybe Word8)

checkReadShow :: forall string number f.
    (Monad f, Eq number, Show number, Eq string, Show string) =>
    Gen number -> (number -> string) -> (string -> Maybe number) -> PropertyT f ()
checkReadShow g s r =
  do
    x <- forAll g
    r (s x) === Just x

prop_readShow_dec_integer_text :: Property
prop_readShow_dec_integer_text = property $
    checkReadShow @T.Text @Integer genInteger Dec.showInteger Dec.readInteger

prop_readShow_dec_integer_string :: Property
prop_readShow_dec_integer_string = property $
    checkReadShow @String @Integer genInteger Dec.showInteger Dec.readInteger

prop_readShow_dec_integer_byteString :: Property
prop_readShow_dec_integer_byteString = property $
    checkReadShow @BS.ByteString @Integer genInteger Dec.showInteger Dec.readInteger

prop_readShow_dec_natural :: Property
prop_readShow_dec_natural = property $
    checkReadShow @T.Text @Natural genNatural Dec.showNatural Dec.readNatural

prop_readShow_dec_word8 :: Property
prop_readShow_dec_word8 = property $
    checkReadShow @T.Text @Word8 genWord8 Dec.showIntegral Dec.readIntegral

prop_readShow_hex_integer_text :: Property
prop_readShow_hex_integer_text = property $ do
    c <- forAll genCase
    checkReadShow @T.Text @Integer genInteger (Hex.showInteger c) Hex.readInteger

prop_readShow_hex_integer_string :: Property
prop_readShow_hex_integer_string = property $ do
    c <- forAll genCase
    checkReadShow @String @Integer genInteger (Hex.showInteger c) Hex.readInteger

prop_readShow_hex_integer_byteString :: Property
prop_readShow_hex_integer_byteString = property $ do
    c <- forAll genCase
    checkReadShow @BS.ByteString @Integer genInteger (Hex.showInteger c) Hex.readInteger

prop_readShow_hex_natural :: Property
prop_readShow_hex_natural = property $ do
    c <- forAll genCase
    checkReadShow @T.Text @Natural genNatural (Hex.showNatural c) Hex.readNatural

prop_readShow_hex_word8 :: Property
prop_readShow_hex_word8 = property $ do
    c <- forAll genCase
    checkReadShow @T.Text @Word8 genWord8 (Hex.showIntegral c) Hex.readIntegral

genNatural :: Gen Natural
genNatural = Gen.integral (Range.exponential 0 10000)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.exponentialFrom 0 (-10000) 10000)

genWord8 :: Gen Word8
genWord8 = Gen.integral (Range.linear minBound maxBound)

genCase :: Gen Case
genCase = Gen.element [UpperCase, LowerCase]
