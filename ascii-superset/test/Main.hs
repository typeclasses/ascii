module Main (main) where

import ASCII.Refinement (ASCII, asciiUnsafe)

import qualified ASCII.Lift as Lift
import qualified ASCII.Refinement as Refinement

import ASCII.Char (Char (..))

import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Function (($))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Text (Text)
import Data.Word (Word8)
import System.Exit (exitFailure)
import System.IO (IO)

import Hedgehog (Property, checkParallel, discover, property, withTests, (===))

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

prop_lift_letter :: Property
prop_lift_letter = withTests 1 $ property $
    (Lift.lift CapitalLetterA :: Word8) === 65

prop_lift_list :: Property
prop_lift_list = withTests 1 $ property $
    (Lift.lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text) === "Hi!"

prop_validate_char :: Property
prop_validate_char = withTests 1 $ property $
    (map Refinement.validateChar [-1, 65, 97, 128] :: [Maybe (ASCII Int)]) === [Nothing, Just (asciiUnsafe 65), Just (asciiUnsafe 97), Nothing]

prop_from_char_list :: Property
prop_from_char_list = withTests 1 $ property $
    (Refinement.fromCharList [CapitalLetterH,SmallLetterI,ExclamationMark] :: ASCII Text) === asciiUnsafe "Hi!"

prop_to_char_list :: Property
prop_to_char_list = withTests 1 $ property $
    Refinement.toCharList (Refinement.substituteString "Piñata" :: ASCII Text) === [CapitalLetterP, SmallLetterI, Substitute, SmallLetterA, SmallLetterT, SmallLetterA]

prop_substitute_string :: Property
prop_substitute_string = withTests 1 $ property $
    (Refinement.substituteString "Cristóbal" :: ASCII Text) === asciiUnsafe "Crist\SUBbal"

prop_validate_text :: Property
prop_validate_text = withTests 1 $ property $
    (map Refinement.validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII Text)]) === [Just (asciiUnsafe "Hello"), Nothing]

prop_validate_string :: Property
prop_validate_string = withTests 1 $ property $
    (map Refinement.validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII String)]) === [Just (asciiUnsafe "Hello"), Nothing]
