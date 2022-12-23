module Main (main) where

import qualified ASCII.Lift as Lift

import ASCII.Char (Char (..))

import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Function (($))
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
