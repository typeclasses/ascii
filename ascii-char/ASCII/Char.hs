{- |

The 'Char' type has 128 nullary constructors, listed in order according to each character's 7-bit numeric code.

-}

module ASCII.Char
  (
    -- * The @Char@ type
    Char (..),

    -- * Conversions with @Int@
    toInt, fromIntMaybe, fromIntUnsafe,

    -- * Enumeration
    allCharacters

    -- * Notes
    -- $notes

  ) where

import Prelude ((<), (>), otherwise, Int, Maybe (..))

import qualified Prelude
import qualified Data.Data as Data
import qualified GHC.Generics as G

{- $setup

>>> import Prelude hiding (Char)

-}

-- | A character in the ASCII character set.

data Char =
      Null | StartOfHeading | StartOfText | EndOfText | EndOfTransmission | Enquiry | Acknowledgement | Bell | Backspace | HorizontalTab | LineFeed | VerticalTab | FormFeed | CarriageReturn | ShiftOut | ShiftIn | DataLinkEscape

    | DeviceControl1 | DeviceControl2 | DeviceControl3 | DeviceControl4

    | NegativeAcknowledgement | SynchronousIdle | EndOfTransmissionBlock | Cancel | EndOfMedium | Substitute | Escape

    | FileSeparator | GroupSeparator | RecordSeparator | UnitSeparator

    | Space | ExclamationMark | QuotationMark | NumberSign | DollarSign | PercentSign | Ampersand | Apostrophe | LeftParenthesis | RightParenthesis | Asterisk | PlusSign | Comma | HyphenMinus | FullStop | Slash

    | Digit0 | Digit1 | Digit2 | Digit3 | Digit4 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9

    | Colon | Semicolon | LessThanSign | EqualsSign | GreaterThanSign | QuestionMark | AtSign

    | CapitalLetterA | CapitalLetterB | CapitalLetterC | CapitalLetterD | CapitalLetterE | CapitalLetterF | CapitalLetterG | CapitalLetterH | CapitalLetterI | CapitalLetterJ | CapitalLetterK | CapitalLetterL | CapitalLetterM | CapitalLetterN | CapitalLetterO | CapitalLetterP | CapitalLetterQ | CapitalLetterR | CapitalLetterS | CapitalLetterT | CapitalLetterU | CapitalLetterV | CapitalLetterW | CapitalLetterX | CapitalLetterY | CapitalLetterZ

    | LeftSquareBracket | Backslash | RightSquareBracket | Caret | Underscore | GraveAccent

    | SmallLetterA | SmallLetterB | SmallLetterC | SmallLetterD | SmallLetterE | SmallLetterF | SmallLetterG | SmallLetterH | SmallLetterI | SmallLetterJ | SmallLetterK | SmallLetterL | SmallLetterM | SmallLetterN | SmallLetterO | SmallLetterP | SmallLetterQ | SmallLetterR | SmallLetterS | SmallLetterT | SmallLetterU | SmallLetterV | SmallLetterW | SmallLetterX | SmallLetterY | SmallLetterZ

    | LeftCurlyBracket | VerticalLine | RightCurlyBracket | Tilde | Delete

deriving instance Prelude.Eq Char
deriving instance Prelude.Ord Char

-- | Instead of @Enum@ methods, consider using 'toInt' and 'fromIntMaybe'.
deriving instance Prelude.Enum Char

-- | The least character is 'Null', and the greatest character is 'Delete'.
deriving instance Prelude.Bounded Char

deriving instance Prelude.Show Char

-- Requires the DeriveDataTypeable language extension.
deriving instance Data.Data Char

-- Requires the DeriveGeneric language extension.
deriving instance G.Generic Char

{- | Converts an ASCII character to its corresponding numeric value between 0 and 127.

>>> map toInt [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

toInt :: Char -> Int
toInt = Prelude.fromEnum

{- | Returns 'Just' the ASCII character corresponding to a numeric value between 0 and 127, or 'Nothing' for numbers outside this range.

>>> map fromIntMaybe [-1, 0, 65, 127, 128]
[Nothing,Just Null,Just CapitalLetterA,Just Delete,Nothing]

-}

fromIntMaybe :: Int -> Maybe Char
fromIntMaybe x | x < 0     = Nothing
               | x > 127   = Nothing
               | otherwise = Just (fromIntUnsafe x)

{- | The inverse of 'toInt'.

This is marked as /unsafe/ because it is undefined for numbers below 0 or above 127. The safe variant of this function is 'fromIntMaybe'.

>>> map fromIntUnsafe [65, 66, 67]
[CapitalLetterA,CapitalLetterB,CapitalLetterC]

-}

fromIntUnsafe :: Int -> Char
fromIntUnsafe = Prelude.toEnum

allCharacters :: [Char]
allCharacters = Prelude.enumFromTo Prelude.minBound Prelude.maxBound

{- $notes

There are 128 characters in total.

>>> length allCharacters
128

Null is the first character.

>>> minBound :: Char
Null

Delete is the last character.

>>> maxBound :: Char
Delete

-}
