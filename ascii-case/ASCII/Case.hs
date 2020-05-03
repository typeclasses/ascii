{- |

/Case/ is a property of letters. /A-Z/ are /upper case/ letters, and /a-z/ are /lower case/ letters. No other ASCII characters have case.

-}

module ASCII.Case ( Case (..), letterCase, isCase ) where

import ASCII.Char ( Char (..) )
import Data.Eq    ( Eq )
import Data.Ord   ( Ord, (<=), (>=) )
import Data.Bool  ( Bool, otherwise )
import Prelude    ( Enum, Bounded )
import Text.Show  ( Show )
import Data.Maybe ( Maybe (..) )

import qualified Data.Bool as Bool

{- $setup

>>> import Prelude

-}

data Case =
    UpperCase -- ^ The letters from 'CapitalLetterA' to 'CapitalLetterZ'.
  | LowerCase -- ^ The letters from 'SmallLetterA' to 'SmallLetterZ'.
  deriving (Eq, Ord, Enum, Bounded, Show)

{- | Determines whether a character is a letter, and if so, whether it is upper or lower case.

>>> map letterCase [CapitalLetterR, SmallLetterR, DollarSign]
[Just UpperCase,Just LowerCase,Nothing]

-}

letterCase :: Char -> Maybe Case
letterCase x | isCase UpperCase x = Just UpperCase
             | isCase LowerCase x = Just LowerCase
             | otherwise          = Nothing

{- | Determines whether a character is a letter of a particular case.

>>> map (isCase UpperCase) [CapitalLetterR,SmallLetterR,DollarSign]
[True,False,False]

-}

isCase :: Case -> Char -> Bool
isCase c x = (Bool.&&) ( x >= a ) ( x <= z ) where (a, z) = az c

az :: Case -> (Char, Char)
az UpperCase = (CapitalLetterA, CapitalLetterZ)
az LowerCase = (SmallLetterA, SmallLetterZ)
