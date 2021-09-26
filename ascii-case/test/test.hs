module Main where

import ASCII.Case

import ASCII.Char (Char (..))

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Function ((.), ($))
import Data.Functor (Functor (..))
import Data.List (intercalate, map, null)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = dieIfFailures $ do
    test 1 $ map letterCase [CapitalLetterR, SmallLetterR, DollarSign] == [Just UpperCase, Just LowerCase,Nothing]
    test 2 $ map (isCase UpperCase) [CapitalLetterR, SmallLetterR, DollarSign] == [True, False, False]
    test 3 $ toCase UpperCase SmallLetterX == CapitalLetterX
    test 4 $ toCase LowerCase CapitalLetterF == SmallLetterF
    test 5 $ toCase UpperCase CapitalLetterA == CapitalLetterA
    test 6 $ toCase UpperCase ExclamationMark == ExclamationMark

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
