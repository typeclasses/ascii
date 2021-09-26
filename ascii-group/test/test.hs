module Main where

import ASCII.Group

import ASCII.Char (Char (..), allCharacters)

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..), not)
import Data.Eq (Eq ((==)))
import Data.Foldable (all)
import Data.Function ((.), ($))
import Data.Functor (Functor (..))
import Data.List (intercalate, map, null, length, filter)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = dieIfFailures $ do
    test 1 $ map charGroup [CapitalLetterA, EndOfTransmission] == [Printable, Control]
    test 2 $ not $ inGroup Printable EndOfTransmission
    test 3 $ inGroup Control EndOfTransmission
    test 4 $ charGroup Space == Printable
    test 5 $ charGroup HorizontalTab == Control
    test 6 $ all (inGroup Printable) [CapitalLetterA, SmallLetterZ, Digit4, Tilde]
    test 7 $ all (inGroup Control) [Null, Substitute, UnitSeparator, Delete]
    test 8 $ length (filter (inGroup Control) allCharacters) == 33
    test 9 $ length (filter (inGroup Printable) allCharacters) == 95

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
