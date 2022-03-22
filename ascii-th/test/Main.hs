module Main (main) where

import ASCII.QuasiQuoters (char, string)
import ASCII.TemplateHaskell (charExp, charListExp, charListPat, charPat,
                              isCharExp, isCharPat)

import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)
import ASCII.Superset (toCharOrFail)

import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Function (($))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude (Integer)
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Data.ByteString.Builder as BS.Builder

import Hedgehog (Property, checkParallel, discover, property, withTests, (===))

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

prop_smallE :: Property
prop_smallE = withTests 1 $ property $
    ([char|e|] :: Char) === SmallLetterE

prop_smallE_word8 :: Property
prop_smallE_word8 = withTests 1 $ property $
    ([char|e|] :: Word8) === 101

prop_tilde_pattern :: Property
prop_tilde_pattern = withTests 1 $ property $
    2 === case Tilde of
        [char|@|] -> 1 :: Integer
        [char|~|] -> 2
        _ -> 3

prop_hello_list :: Property
prop_hello_list = withTests 1 $ property $
    ([string|Hello!|] :: [Char]) === [CapitalLetterH, SmallLetterE, SmallLetterL, SmallLetterL, SmallLetterO, ExclamationMark]

prop_hello_string :: Property
prop_hello_string = withTests 1 $ property $
    ([string|Hello!|] :: String) === "Hello!"

prop_hello_text :: Property
prop_hello_text = withTests 1 $ property $
    ([string|Hello!|] :: Text) === "Hello!"

prop_string_qq_expression :: Property
prop_string_qq_expression = withTests 1 $ property $
    BS.Builder.toLazyByteString [string|Hello!|] === "Hello!"

prop_string_qq_pattern :: Property
prop_string_qq_pattern = withTests 1 $ property $
    2 === case [CapitalLetterH, SmallLetterI] of
        [string|Bye|] -> 1 :: Integer
        [string|Hi|] -> 2
        _ -> 3

prop_char_splice_letter :: Property
prop_char_splice_letter = withTests 1 $ property $
    $(toCharOrFail 'F' >>= charExp) === CapitalLetterF

prop_char_splice_del :: Property
prop_char_splice_del = withTests 1 $ property $
    $(toCharOrFail '\DEL' >>= charExp) === Delete

prop_char_splice_pattern :: Property
prop_char_splice_pattern = withTests 1 $ property $
    2 === case SmallLetterS of
        $(toCharOrFail 'r' >>= charPat) -> 1 :: Integer
        $(toCharOrFail 's' >>= charPat) -> 2
        _ -> 3

prop_char_list_splice :: Property
prop_char_list_splice = withTests 1 $ property $
    $(charListExp [CapitalLetterH, SmallLetterI]) === [CapitalLetterH, SmallLetterI]

prop_char_list_splice_pattern :: Property
prop_char_list_splice_pattern = withTests 1 $ property $
    2 === case [CapitalLetterH, SmallLetterI] of
        $(charListPat [CapitalLetterH, SmallLetterA]) -> 1 :: Integer
        $(charListPat [CapitalLetterH, SmallLetterI]) -> 2
        _ -> 3

prop_polymorphic_char_splice :: Property
prop_polymorphic_char_splice = withTests 1 $ property $
    ($(isCharExp CapitalLetterA) :: Char) === CapitalLetterA

prop_polymorphic_char_splice_word8 :: Property
prop_polymorphic_char_splice_word8 = withTests 1 $ property $
    ($(isCharExp CapitalLetterA) :: Word8) === 65

prop_polymorphic_char_splice_ascii_word8 :: Property
prop_polymorphic_char_splice_ascii_word8 = withTests 1 $ property $
    ($(isCharExp CapitalLetterA) :: ASCII Word8) === asciiUnsafe 65

prop_polymorphic_char_splice_pattern :: Property
prop_polymorphic_char_splice_pattern = withTests 1 $ property $
    2 === case (66 :: Word8) of
        $(isCharPat CapitalLetterA) -> 1 :: Integer
        $(isCharPat CapitalLetterB) -> 2
        _ -> 3
