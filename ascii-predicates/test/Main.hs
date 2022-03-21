module Main (main) where

import ASCII.ListsAndPredicates

import qualified ASCII.Char

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Function ((.), ($))
import Data.Functor (Functor (..))
import Data.List (filter, intercalate, map, null)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = dieIfFailures $ do

    do
        let lists = [ all, printableCharacters, controlCodes, letters,
                      capitalLetters, smallLetters, digits, octDigits,
                      hexDigits, numbers]

        test 1 $ List.all (\xs -> List.sort xs == xs) lists

    do
        let convert = Char.chr . ASCII.Char.toInt
            eq f g = List.all (\x -> f x == g (convert x))
                              ASCII.Char.allCharacters

        test 2 $ eq isControl Char.isControl
        test 3 $ eq isSpace Char.isSpace
        test 4 $ eq isLower Char.isLower
        test 5 $ eq isUpper Char.isUpper
        test 6 $ eq isAlpha Char.isAlpha
        test 7 $ eq isAlphaNum Char.isAlphaNum
        test 8 $ eq isPrint Char.isPrint
        test 9 $ eq isDigit Char.isDigit
        test 10 $ eq isOctDigit Char.isOctDigit
        test 11 $ eq isHexDigit Char.isHexDigit
        test 12 $ eq isLetter Char.isLetter
        test 13 $ eq isMark Char.isMark
        test 14 $ eq isNumber Char.isNumber
        test 15 $ eq isPunctuation Char.isPunctuation
        test 16 $ eq isSymbol Char.isSymbol
        test 17 $ eq isSeparator Char.isSeparator

    test 18 $ controlCodes == filter isControl all
    test 19 $ printableCharacters == filter isPrint all
    test 20 $ letters == filter isLetter all
    test 21 $ capitalLetters == filter isUpper all
    test 22 $ smallLetters == filter isLower all
    test 23 $ digits == filter isDigit all
    test 24 $ numbers == filter isNumber all
    test 25 $ octDigits == filter isOctDigit all
    test 26 $ hexDigits == filter isHexDigit all

dieIfFailures :: Failures a -> IO a
dieIfFailures (Failures fs x) =
    if null fs
        then do putStrLn "💯"; return x
        else die $ intercalate " " (map (("🔥" <> ) . show) fs)

type TestNumber = Natural

test :: TestNumber -> Bool -> Failures ()
test n t = Failures (if t then [] else [n]) ()

data Failures a = Failures [TestNumber] a

instance Functor Failures
  where
    fmap f (Failures a x) = Failures a (f x)

instance Applicative Failures
  where
    pure x = Failures [] x
    Failures a f <*> Failures b x = Failures (a <> b) (f x)

instance Monad Failures
  where
    Failures a x >>= f = let Failures b y = f x in Failures (a <> b) y
