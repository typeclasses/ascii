module Main (main) where

import ASCII

import ASCII.Char (Char (..))
import ASCII.Refinement (asciiUnsafe)

import Control.Monad (Monad (..), when)
import Data.Bool (Bool (..), not)
import Data.Function (($))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Word (Word8)
import System.Exit (exitFailure)
import System.IO (IO)

import Hedgehog (Property, assert, checkParallel, discover, property, withTests,
                 (===))

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

prop_letter_printable :: Property
prop_letter_printable = withTests 1 $ property $
    charGroup CapitalLetterA === Printable

prop_eot_control :: Property
prop_eot_control = withTests 1 $ property $
    charGroup EndOfTransmission === Control

prop_eot_not_printable :: Property
prop_eot_not_printable = withTests 1 $ property $
    assert $ not $ inGroup Printable EndOfTransmission

prop_is_eot_control :: Property
prop_is_eot_control = withTests 1 $ property $
    assert $ inGroup Control EndOfTransmission

prop_inGroup_printable :: Property
prop_inGroup_printable = withTests 1 $ property $
    map (inGroup Printable) ([-1, 5, 65, 97, 127, 130] :: [Int]) === [False, False, True, True, False, False]

prop_cases :: Property
prop_cases = withTests 1 $ property $
    map letterCase [SmallLetterA, CapitalLetterA, ExclamationMark] === [Just LowerCase, Just UpperCase, Nothing]

prop_cases_string_qq :: Property
prop_cases_string_qq = withTests 1 $ property $
    map letterCase ([string|Hey!|] :: [ASCII Word8]) === [Just UpperCase, Just LowerCase, Just LowerCase, Nothing]

prop_is_upper_char :: Property
prop_is_upper_char = withTests 1 $ property $
    map (isCase UpperCase) [SmallLetterA, CapitalLetterA, ExclamationMark] === [False, True, False]

prop_is_upper_string_qq :: Property
prop_is_upper_string_qq = withTests 1 $ property $
    map (isCase UpperCase) ([string|Hey!|] :: [ASCII Word8]) === [True, False, False, False]

prop_is_upper_int :: Property
prop_is_upper_int = withTests 1 $ property $
    map (isCase UpperCase) ([-1, 65, 97, 150] :: [Int]) === [False, True, False, False]

prop_to_upper_letter :: Property
prop_to_upper_letter = withTests 1 $ property $
    toCaseChar UpperCase SmallLetterA === CapitalLetterA

prop_to_upper_char_qq :: Property
prop_to_upper_char_qq = withTests 1 $ property $
    ([char|a|] :: ASCII Word8, toCaseChar UpperCase [char|a|] :: ASCII Word8) === (asciiUnsafe 97, asciiUnsafe 65)

prop_to_upper_various :: Property
prop_to_upper_various = withTests 1 $ property $
    toCaseString UpperCase [CapitalLetterH,SmallLetterE,SmallLetterY,ExclamationMark] === [CapitalLetterH, CapitalLetterE, CapitalLetterY, ExclamationMark]

prop_to_upper_string_qq :: Property
prop_to_upper_string_qq = withTests 1 $ property $
    (toCaseString UpperCase [string|Hey!|] :: ASCII Text) === asciiUnsafe "HEY!"

prop_to_int :: Property
prop_to_int = withTests 1 $ property $
    map charToInt [Null, CapitalLetterA, SmallLetterA, Delete] === [0, 65, 97, 127]

prop_to_word8 :: Property
prop_to_word8 = withTests 1 $ property $
    map charToWord8 [Null, CapitalLetterA, SmallLetterA, Delete] === [0, 65, 97, 127]

prop_to_text :: Property
prop_to_text = withTests 1 $ property $
    charListToText [CapitalLetterH, SmallLetterI, ExclamationMark] === "Hi!"

prop_lift_to_word8 :: Property
prop_lift_to_word8 = withTests 1 $ property $
    (lift CapitalLetterA :: Word8) === 65

prop_lift_to_text :: Property
prop_lift_to_text = withTests 1 $ property $
    (lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text) === "Hi!"

prop_bytes_to_string :: Property
prop_bytes_to_string = withTests 1 $ property $
    ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x50] === Just "HTTP"

prop_bytes_to_string_fail :: Property
prop_bytes_to_string_fail = withTests 1 $ property $
    ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x80] === Nothing
