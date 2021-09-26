module Main where

import ASCII.Char

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Function ((.), ($))
import Data.Functor (Functor (..))
import Data.List (intercalate, map, null, length)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import Prelude (minBound, maxBound)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = dieIfFailures $ do
    test 1 $ map toInt [Null, CapitalLetterA, SmallLetterA, Delete] == [0, 65, 97, 127]
    test 2 $ map fromIntMaybe [-1, 0, 65, 127, 128] == [Nothing, Just Null, Just CapitalLetterA, Just Delete, Nothing]
    test 3 $ map fromIntUnsafe [65, 66, 67] == [CapitalLetterA, CapitalLetterB, CapitalLetterC]
    test 4 $ length allCharacters == 128
    test 5 $ (minBound :: Char) == Null
    test 6 $ (maxBound :: Char) == Delete

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
