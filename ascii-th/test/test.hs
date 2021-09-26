module Main where

import ASCII.QuasiQuoters
import ASCII.TemplateHaskell

import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)
import ASCII.Superset (toCharOrFail)

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Function ((.), ($))
import Data.Functor (Functor (..))
import Data.List (intercalate, map, null)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Prelude (Integer)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

import qualified Data.ByteString.Builder as BS.Builder

main :: IO ()
main = dieIfFailures $ do
    test 1 $ ([char|e|] :: Char) == SmallLetterE
    test 2 $ ([char|e|] :: Word8) == 101
    test 3 $ (case Tilde of [char|@|] -> 1 :: Integer; [char|~|] -> 2; _ -> 3) == 2
    test 4 $ ([string|Hello!|] :: [Char]) == [CapitalLetterH, SmallLetterE, SmallLetterL, SmallLetterL, SmallLetterO, ExclamationMark]
    test 5 $ ([string|Hello!|] :: String) == "Hello!"
    test 6 $ ([string|Hello!|] :: Text) == "Hello!"
    test 7 $ (BS.Builder.toLazyByteString [string|Hello!|] == "Hello!")
    test 8 $ (case [CapitalLetterH, SmallLetterI] of [string|Bye|] -> 1 :: Integer; [string|Hi|] -> 2; _ -> 3) == 2
    test 9 $ $(toCharOrFail 'F' >>= charExp) == CapitalLetterF
    test 10 $ $(toCharOrFail '\DEL' >>= charExp) == Delete
    test 11 $ case SmallLetterS of { $(toCharOrFail 'r' >>= charPat) -> 1 :: Integer; $(toCharOrFail 's' >>= charPat) -> 2; _ -> 3 } == 2
    test 12 $ $(charListExp [CapitalLetterH, SmallLetterI]) == [CapitalLetterH, SmallLetterI]
    test 13 $ case [CapitalLetterH, SmallLetterI] of { $(charListPat [CapitalLetterH, SmallLetterA]) -> 1 :: Integer; $(charListPat [CapitalLetterH, SmallLetterI]) -> 2; _ -> 3 } == 2
    test 14 $ ($(isCharExp CapitalLetterA) :: Char) == CapitalLetterA
    test 15 $ ($(isCharExp CapitalLetterA) :: Word8) == 65
    test 16 $ ($(isCharExp CapitalLetterA) :: ASCII Word8) == asciiUnsafe 65
    test 17 $ case (66 :: Word8) of { $(isCharPat CapitalLetterA) -> 1 :: Integer; $(isCharPat CapitalLetterB) -> 2; _ -> 3 } == 2

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
