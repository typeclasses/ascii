{- |

/Case/ is a property of letters. /A-Z/ are /upper case/ letters, and /a-z/ are /lower case/ letters. No other ASCII characters have case.

-}

module ASCII.Case ( Case (..), letterCase, isCase, toCase ) where

import ASCII.Char    ( Char (..) )
import Data.Bool     ( Bool, otherwise )
import Data.Data     ( Data )
import Data.Eq       ( Eq )
import Data.Function ( (.), ($) )
import Data.Hashable ( Hashable )
import Data.Ord      ( Ord, (<=), (>=) )
import GHC.Generics  ( Generic )
import Prelude       ( Enum, Bounded, Int, (+), (-) )
import Text.Show     ( Show )
import Data.Maybe    ( Maybe (..) )

import qualified ASCII.Char as Char
import qualified Data.Bool  as Bool

{- $setup

>>> import Prelude

-}

data Case =
    UpperCase -- ^ The letters from 'CapitalLetterA' to 'CapitalLetterZ'.
  | LowerCase -- ^ The letters from 'SmallLetterA' to 'SmallLetterZ'.

deriving stock instance Eq Case

deriving stock instance Ord Case

deriving stock instance Enum Case

deriving stock instance Bounded Case

deriving stock instance Show Case

deriving stock instance Data Case

deriving stock instance Generic Case

deriving anyclass instance Hashable Case

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

{- | Maps a letter character to its upper/lower case equivalent.

>>> toCase UpperCase SmallLetterX
CapitalLetterX

>>> toCase LowerCase CapitalLetterF
SmallLetterF

Characters that are already in the requested case are unmodified by this transformation.

>>> toCase UpperCase CapitalLetterA
CapitalLetterA

Characters that are not letters, such as exclamation mark, are unmodified by this transformation.

>>> toCase UpperCase ExclamationMark
ExclamationMark

-}

toCase :: Case -> Char -> Char
toCase c x = if isCase (opposite c) x then changeCaseUnsafe c x else x

-- | Change a letter to the given case, assuming that the input character is a letter of the opposite case.

changeCaseUnsafe :: Case -> Char -> Char
changeCaseUnsafe c = charAsIntUnsafe (changeCaseInt c)

changeCaseInt :: Case -> Int -> Int
changeCaseInt LowerCase i = i + 32
changeCaseInt UpperCase i = i - 32

opposite UpperCase = LowerCase
opposite LowerCase = UpperCase

charAsIntUnsafe :: (Int -> Int) -> (Char -> Char)
charAsIntUnsafe f = Char.fromIntUnsafe . f . Char.toInt
