{-# OPTIONS_GHC -Wall #-}

{- |

The __American Standard Code for Information Interchange__ (ASCII) comprises a set of 128 characters, each represented by 7 bits. 33 of these characters are /control codes/; a few of these are still in use, but most are obsolete relics of the early days of computing. The other 95 are /printable characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as UTF-8 that can represent the entire Unicode character set, which includes over a hundred thousand characters and is not limited to the symbols of any particular writing system or culture. However, ASCII is still relevant to network protocols; for example, we can see it in the specification of [HTTP message headers](https://tools.ietf.org/html/rfc7230#section-1.2).

There is a convenient relationship between ASCII and Unicode: the ASCII characters are the first 128 characters of the much larger Unicode character set. The [C0 Controls and Basic Latin](https://www.unicode.org/charts/PDF/U0000.pdf) section of the Unicode standard contains a list of all the ASCII characters. You may also find this list replicated below; each ASCII character corresponds to a constructor of the 'Char' type.

We do not elaborate on the semantics of the control characters here, because this information is both obsolete and restricted by copyright law. It is described by a document entitled /Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)/, published by American National Standards Institute (ANSI) and available for purchase [on their website](https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012).

-}

module ASCII
  (

  -- * Characters
    Char (..)
  -- ** General integral conversions
  -- $integralConversions
  , integralChar, integralCharMaybe, charIntegral
  -- ** Int conversions
  , charInt, intChar, intCharMaybe
  -- ** Word8 conversions
  , word8Char, word8CharMaybe, charWord8

  -- * Strings
  , String
  -- ** [Char] conversions
  , pack, unpack

  ) where

import Prelude (Bool, Eq, Enum, Bounded, Show, Ord (..), (&&))
import Data.Function ((.))

import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Integral (..), Int, fromIntegral)
import qualified Prelude as May (Maybe (..))
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Data.Array.Unboxed as Array


---  Individual characters  ---

data Char =
      Null
    | StartOfHeading
    | StartOfText
    | EndOfText
    | EndOfTransmission
    | Enquiry
    | Acknowledgement
    | Bell
    | Backspace
    | HorizontalTab
    | LineFeed
    | VerticalTab
    | FormFeed
    | CarriageReturn
    | ShiftOut
    | ShiftIn
    | DataLinkEscape
    | DeviceControl1
    | DeviceControl2
    | DeviceControl3
    | DeviceControl4
    | NegativeAcknowledgement
    | SynchronousIdle
    | EndOfTransmissionBlock
    | Cancel
    | EndOfMedium
    | Substitute
    | Escape
    | FileSeparator
    | GroupSeparator
    | RecordSeparator
    | UnitSeparator
    | Space
    | ExclamationMark
    | QuotationMark
    | NumberSign
    | DollarSign
    | PercentSign
    | Ampersand
    | Apostrophe
    | LeftParenthesis
    | RightParenthesis
    | Asterisk
    | PlusSign
    | Comma
    | HyphenMinus
    | FullStop
    | Slash
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Colon
    | Semicolon
    | LessThanSign
    | EqualsSign
    | GreaterThanSign
    | QuestionMark
    | AtSign
    | CapitalLetterA
    | CapitalLetterB
    | CapitalLetterC
    | CapitalLetterD
    | CapitalLetterE
    | CapitalLetterF
    | CapitalLetterG
    | CapitalLetterH
    | CapitalLetterI
    | CapitalLetterJ
    | CapitalLetterK
    | CapitalLetterL
    | CapitalLetterM
    | CapitalLetterN
    | CapitalLetterO
    | CapitalLetterP
    | CapitalLetterQ
    | CapitalLetterR
    | CapitalLetterS
    | CapitalLetterT
    | CapitalLetterU
    | CapitalLetterV
    | CapitalLetterW
    | CapitalLetterX
    | CapitalLetterY
    | CapitalLetterZ
    | LeftSquareBracket
    | Backslash
    | RightSquareBracket
    | Caret
    | Underscore
    | GraveAccent
    | SmallLetterA
    | SmallLetterB
    | SmallLetterC
    | SmallLetterD
    | SmallLetterE
    | SmallLetterF
    | SmallLetterG
    | SmallLetterH
    | SmallLetterI
    | SmallLetterJ
    | SmallLetterK
    | SmallLetterL
    | SmallLetterM
    | SmallLetterN
    | SmallLetterO
    | SmallLetterP
    | SmallLetterQ
    | SmallLetterR
    | SmallLetterS
    | SmallLetterT
    | SmallLetterU
    | SmallLetterV
    | SmallLetterW
    | SmallLetterX
    | SmallLetterY
    | SmallLetterZ
    | LeftCurlyBracket
    | VerticalLine
    | RightCurlyBracket
    | Tilde
    | Delete
  deriving (Eq, Ord, Enum, Bounded, Show)


---  Direct usage of the Enum instance  ---

-- | Specialization of 'integralCharUnsafe'.
intCharUnsafe :: Num.Int -> Char
intCharUnsafe = Enum.toEnum

-- | Specialization of 'charIntegral'.
charInt :: Char -> Num.Int
charInt = Enum.fromEnum


---  Safe bound-checked usage of the Enum instance  ---

intInRange :: Num.Int -> Bool
intInRange x = (x >= 0) && (x <= 127)

-- | Specialization of 'integralChar'.
intChar :: Num.Int -> Char
intChar x = if intInRange x then intCharUnsafe x else Substitute

-- | Specialization of 'integralCharMaybe'.
intCharMaybe :: Num.Int -> May.Maybe Char
intCharMaybe x = if intInRange x then May.Just (intCharUnsafe x) else May.Nothing


---  Integral generalizations  ---

-- $integralConversions
-- These functions are conversions between a 'Char' and its corresponding number between 0 and 127. This module also contains specializations of these functions for 'Num.Int' and 'Word.Word8' in case you find those variants easier to use than their polymorphic counterparts.

-- | Converts a number between 0 and 127 to its corresponding ASCII 'Char'. Returns the 'Substitute' character for any numbers outside of this range.
integralChar :: Num.Integral int => int -> Char
integralChar = intChar . Num.fromIntegral

-- | Converts a number between 0 and 127 to its corresponding ASCII 'Char'. Returns 'Nothing' for any numbers outside of this range.
integralCharMaybe :: Num.Integral int => int -> May.Maybe Char
integralCharMaybe = intCharMaybe . Num.fromIntegral

-- | Converts a number between 0 and 127 to its corresponding ASCII 'Char'. Behavior is undefined for any numbers outside of this range.
integralCharUnsafe :: Num.Integral int => int -> Char
integralCharUnsafe = intCharUnsafe . Num.fromIntegral

-- | Converts an ASCII 'Char' to its corresponding number between 0 and 127.

charIntegral :: Num.Integral int => Char -> int
charIntegral = Num.fromIntegral . charInt


---  Word8 specializations  ---

-- | Specialization of 'integralChar'.
word8Char :: Word.Word8 -> Char
word8Char = integralChar

-- | Specialization of 'integralCharMaybe'.
word8CharMaybe :: Word.Word8 -> May.Maybe Char
word8CharMaybe = integralCharMaybe

-- | Specialization of 'integralCharUnsafe'.
word8CharUnsafe :: Word.Word8 -> Char
word8CharUnsafe = integralCharUnsafe

-- | Specialization of 'charIntegral'.
charWord8 :: Char -> Word.Word8
charWord8 = charIntegral


---  Strings  --

newtype String = String { stringArray :: Array.UArray Num.Int Word.Word8 }

pack :: [Char] -> String
pack = String . word8ListArray . List.map charWord8

word8ListArray :: [Word.Word8] -> Array.UArray Num.Int Word.Word8
word8ListArray xs = Array.listArray (1, List.length xs) xs

unpack :: String -> [Char]
unpack = List.map word8CharUnsafe . Array.elems . stringArray
