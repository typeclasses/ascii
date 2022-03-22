module Main (main) where

import ASCII.Refinement (ASCII, asciiUnsafe)

import qualified ASCII.Lift as Lift
import qualified ASCII.Refinement as Refinement

import ASCII.Char (Char (..))

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
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = dieIfFailures $ do

    test 1 $ (Lift.lift CapitalLetterA :: Word8) == 65
    test 2 $ (Lift.lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text) == "Hi!"

    test 3 $ (map Refinement.validateChar [-1, 65, 97, 128] :: [Maybe (ASCII Int)]) == [Nothing, Just (asciiUnsafe 65), Just (asciiUnsafe 97), Nothing]
    test 4 $ (Refinement.fromCharList [CapitalLetterH,SmallLetterI,ExclamationMark] :: ASCII Text) == asciiUnsafe "Hi!"
    test 5 $ Refinement.toCharList (Refinement.substituteString "Piñata" :: ASCII Text) == [CapitalLetterP, SmallLetterI, Substitute, SmallLetterA, SmallLetterT, SmallLetterA]
    test 6 $ (Refinement.substituteString "Cristóbal" :: ASCII Text) == asciiUnsafe "Crist\SUBbal"
    test 7 $ (map Refinement.validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII Text)]) == [Just (asciiUnsafe "Hello"), Nothing]
    test 8 $ (map Refinement.validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII String)]) == [Just (asciiUnsafe "Hello"), Nothing]

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
