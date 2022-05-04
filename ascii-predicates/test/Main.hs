module Main (main) where

import ASCII.ListsAndPredicates

import qualified ASCII.Char

import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Eq (Eq ((==)))
import Data.Function (($), (.))
import Data.List (filter)
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Data.Char as Char
import qualified Data.List as List

import Hedgehog (MonadTest, Property, assert, checkParallel, discover, property,
                 withTests, (===))

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

---

-- This test ensures that all of the lists give characters in ascending order.

lists :: [[ASCII.Char.Char]]
lists = [ all, printableCharacters, controlCodes, letters,
          capitalLetters, smallLetters, digits, octDigits,
          hexDigits, numbers, visibleCharacters ]

prop_lists_are_sorted :: Property
prop_lists_are_sorted = withTests 1 $ property $
    assert $ List.all (\xs -> List.sort xs == xs) lists

---

-- These tests check that the predicates in our package that have corresponding functions in `base` behave the same as their counterparts.

eq :: (MonadTest m, Eq a) => (ASCII.Char.Char -> a) -> (Char.Char -> a) -> m ()
eq f g = assert $ List.all (\x -> f x == g (convert x)) ASCII.Char.allCharacters
  where
    convert = Char.chr . ASCII.Char.toInt

prop_eq_control :: Property
prop_eq_control = withTests 1 $ property $
    eq isControl Char.isControl

prop_eq_space :: Property
prop_eq_space = withTests 1 $ property $
    eq isSpace Char.isSpace

prop_eq_lower :: Property
prop_eq_lower = withTests 1 $ property $
    eq isLower Char.isLower

prop_eq_upper :: Property
prop_eq_upper = withTests 1 $ property $
    eq isUpper Char.isUpper

prop_eq_alpha :: Property
prop_eq_alpha = withTests 1 $ property $
    eq isAlpha Char.isAlpha

prop_eq_alphaNum :: Property
prop_eq_alphaNum = withTests 1 $ property $
    eq isAlphaNum Char.isAlphaNum

prop_eq_print :: Property
prop_eq_print = withTests 1 $ property $
    eq isPrint Char.isPrint

prop_eq_digit :: Property
prop_eq_digit = withTests 1 $ property $
    eq isDigit Char.isDigit

prop_eq_octDigit :: Property
prop_eq_octDigit = withTests 1 $ property $
    eq isOctDigit Char.isOctDigit

prop_eq_hexDigit :: Property
prop_eq_hexDigit = withTests 1 $ property $
    eq isHexDigit Char.isHexDigit

prop_eq_letter :: Property
prop_eq_letter = withTests 1 $ property $
    eq isLetter Char.isLetter

prop_eq_mark :: Property
prop_eq_mark = withTests 1 $ property $
    eq isMark Char.isMark

prop_eq_number :: Property
prop_eq_number = withTests 1 $ property $
    eq isNumber Char.isNumber

prop_eq_punctuation :: Property
prop_eq_punctuation = withTests 1 $ property $
    eq isPunctuation Char.isPunctuation

prop_eq_symbol :: Property
prop_eq_symbol = withTests 1 $ property $
    eq isSymbol Char.isSymbol

prop_eq_separator :: Property
prop_eq_separator = withTests 1 $ property $
    eq isSeparator Char.isSeparator

---

-- These tests ensure that the predicates are coherent with the lists.

prop_list_control :: Property
prop_list_control = withTests 1 $ property $
    controlCodes === filter isControl all

prop_list_printable :: Property
prop_list_printable = withTests 1 $ property $
    printableCharacters === filter isPrint all

prop_list_letter :: Property
prop_list_letter = withTests 1 $ property $
    letters === filter isLetter all

prop_list_capital :: Property
prop_list_capital = withTests 1 $ property $
    capitalLetters === filter isUpper all

prop_list_small :: Property
prop_list_small = withTests 1 $ property $
    smallLetters === filter isLower all

prop_list_digit :: Property
prop_list_digit = withTests 1 $ property $
    digits === filter isDigit all

prop_list_number :: Property
prop_list_number = withTests 1 $ property $
    numbers === filter isNumber all

prop_list_oct :: Property
prop_list_oct = withTests 1 $ property $
    octDigits === filter isOctDigit all

prop_list_hex :: Property
prop_list_hex = withTests 1 $ property $
    hexDigits === filter isHexDigit all

prop_list_visible :: Property
prop_list_visible = withTests 1 $ property $
    visibleCharacters === filter isVisible all
