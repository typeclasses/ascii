{- |

The 'Char' type has 128 nullary constructors, listed in order according to each character's 7-bit numeric code.

-}

module ASCII.Char
  (
    {- * The @Char@ type -} Char (..),
    {- * Conversions with @Int@ -} toInt, fromIntMaybe, fromIntUnsafe,
    {- * Enumeration -} allCharacters
    {- * Notes -} {- $notes -}
  )
  where

import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Eq (Eq, (/=), (==))
import Data.Hashable (Hashable)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord, (<), (>))
import GHC.Generics (Generic)
import Prelude (Bounded, Enum, enumFromTo, fromEnum, maxBound, minBound, toEnum)
import Text.Show (Show)

import qualified Data.Char as C

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

-- | ASCII characters can be compared for equality using '(==)'. Comparisons are case-sensitive; @'SmallLetterA' '/=' 'CapitalLetterA'@.
deriving stock instance Eq Char

-- | ASCII characters are ordered; for example, the letter /A/ is "less than" ('<') the letter /B/ because it appears earlier in the list. The ordering of ASCII characters is the same as the ordering of the corresponding Unicode 'C.Char's.
deriving stock instance Ord Char

-- | The 'Enum' instance allows us to use range syntax, for example @['SmallLetterA' .. 'SmallLetterZ']@ is a list all lower-case letters from /a/ to /z/. Instead of 'toEnum' and 'fromEnum', consider using 'toInt' and 'fromIntMaybe'.
deriving stock instance Enum Char

-- | The least character is 'Null', and the greatest character is 'Delete'. You can write @(['minBound' .. 'maxBound'] :: [ASCII.'Char'])@ to get a list of all the ASCII characters.
deriving stock instance Bounded Char

-- | 'show' produces the name of a constructor. For example, the character @e@ is shown as “@SmallLetterE@”. See "ASCII.Char" for the complete list of constructor names.
deriving stock instance Show Char

-- | The 'Data' instance allows ASCII characters to be used with generic programming in the “SYB” style. (See the <https://hackage.haskell.org/package/syb syb> package and the 2003 paper <https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf Scrap Your Boilerplate> by Ralf Lämmel and Simon Peyton Jones.)
deriving stock instance Data Char

-- | The 'Generic' instance allows ASCII characters to be used with generic programming in the “generic deriving” style. (See the <https://hackage.haskell.org/package/generic-data generic-data> package and the 2010 paper <http://dreixel.net/research/pdf/gdmh.pdf A generic deriving mechanism for Haskell> by José Pedro Magalhães, Atze Dijkstra, Johan Jeuring, and Andres Löh.)
deriving stock instance Generic Char

-- | The 'Hashable' instance lets us collect ASCII characters in hash-based sets, and it lets us use ASCII characters as keys in hash-based maps. (See the @unordered-containers@ package.)
deriving anyclass instance Hashable Char

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
