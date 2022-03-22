module Main (main) where

import ASCII

import ASCII.Char (Char (..))
import ASCII.Refinement (asciiUnsafe)

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Function (($), (.))
import Data.Functor (Functor (..))
import Data.Int (Int)
import Data.List (intercalate, map, null)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = dieIfFailures $ do
    test 1 $ map charGroup [CapitalLetterA, EndOfTransmission] == [Printable, Control]
    test 2 $ inGroup Printable EndOfTransmission == False
    test 3 $ inGroup Control EndOfTransmission == True
    test 4 $ map (inGroup Printable) ([-1, 5, 65, 97, 127, 130] :: [Int]) == [False, False, True, True, False, False]
    test 5 $ map letterCase [SmallLetterA, CapitalLetterA, ExclamationMark] == [Just LowerCase, Just UpperCase, Nothing]
    test 6 $ map letterCase ([string|Hey!|] :: [ASCII Word8]) == [Just UpperCase, Just LowerCase, Just LowerCase, Nothing]
    test 7 $ map (isCase UpperCase) [SmallLetterA, CapitalLetterA, ExclamationMark] == [False, True, False]
    test 8 $ map (isCase UpperCase) ([string|Hey!|] :: [ASCII Word8]) == [True, False, False, False]
    test 9 $ map (isCase UpperCase) ([-1, 65, 97, 150] :: [Int]) == [False, True, False, False]
    test 10 $ toCaseChar UpperCase SmallLetterA == CapitalLetterA
    test 11 $ ([char|a|] :: ASCII Word8, toCaseChar UpperCase [char|a|] :: ASCII Word8) == (asciiUnsafe 97, asciiUnsafe 65)
    test 12 $ toCaseString UpperCase [CapitalLetterH,SmallLetterE,SmallLetterY,ExclamationMark] == [CapitalLetterH, CapitalLetterE, CapitalLetterY, ExclamationMark]
    test 13 $ (toCaseString UpperCase [string|Hey!|] :: ASCII Text) == asciiUnsafe "HEY!"
    test 14 $ map charToInt [Null, CapitalLetterA, SmallLetterA, Delete] == [0, 65, 97, 127]
    test 15 $ map charToWord8 [Null, CapitalLetterA, SmallLetterA, Delete] == [0, 65, 97, 127]
    test 16 $ charListToText [CapitalLetterH, SmallLetterI, ExclamationMark] == "Hi!"
    test 17 $ (lift CapitalLetterA :: Word8) == 65
    test 18 $ (lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text) == "Hi!"
    test 19 $ ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x50] == Just "HTTP"
    test 20 $ ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x80] == Nothing

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
