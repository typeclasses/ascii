{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude, DeriveLift, TypeFamilyDependencies #-}

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

  -- * Char encoding
  , CharEncoding (..), substitute

  -- * Strings
  , String

  -- * String-[Char] conversions
  , pack, unpack

  -- * Character groups
  , Group ( .. ), inGroup, charGroup

  -- * Upper/lower case letters
  , Case (..), isCase, letterCase, CaseInsensitiveEquivalence ( .. ), CaseConversion ( .. )

  -- * Unicode conversion
  , Unicode, UnicodeConversion ( .. )

  ) where

import Prelude ( Bounded, Ord (..), fmap )
import Data.Function ( (.) )
import Data.Eq ( Eq ( (==) ) )

-- Various types of numbers
import Data.Int ( Int )
import Data.Word ( Word8 )

-- Miscellaneous number-related functions
import qualified Prelude as Num ( fromIntegral, (+), (-) )

-- We provide conversions to/from unicode representations of characters; the unicode types are the standard Char and String types from the base module.
import qualified Data.Char as Unicode
import qualified Data.String as Unicode

-- The String type we define here has a custom (not derived) Show instance.
import qualified Text.Show as Show
import Text.Show ( Show )

-- The auto-generated Enum instance for Char makes it easy for us to write the encoding/decoding functions.
import Prelude ( Enum )
import qualified Prelude as Enum ( Enum (..) )

-- Case-insensitivity is expressed as an equivalence.
import qualified Data.Functor.Contravariant as Eq ( Equivalence ( .. ), defaultEquivalence )
import qualified Data.Functor.Contravariant as Contra ( Contravariant (contramap) )

-- True and false, thanks to George Boole.
import qualified Data.Bool as Bool
import Data.Bool ( Bool ( .. ) )

-- Traversing is looping!
import Data.Traversable ( traverse )

-- Maybe is the type of a decoding result, because decoding can fail.
import Data.Maybe ( Maybe )
import qualified Data.Maybe as May ( Maybe ( .. ), fromMaybe )

-- The classic linked list type, [].
import qualified Data.List as List

-- Arrays are for tightly-packed sequences of bytes.
import qualified Data.Array.Unboxed as Array

-- We give the Char type an instance of the Lift class from Template Haskell for the sake of the quasi-quoter in the ASCII.QQ module.
import qualified Language.Haskell.TH.Syntax as TH ( Lift )


---  Individual characters  ---

-- | A character in the ASCII character set.
--
-- This type has 128 nullary constructors, listed in order according to each character's 7-bit numeric code. Its derived 'Enum' instance can therefore be used to convert a 'Char' to its ASCII code and vice versa; but since 'Enum.toEnum' is a partial function, we recommend instead using 'decodeChar', which cannot produce runtime errors.

data Char =
      Null | StartOfHeading | StartOfText | EndOfText | EndOfTransmission | Enquiry | Acknowledgement | Bell | Backspace | HorizontalTab | LineFeed | VerticalTab | FormFeed | CarriageReturn | ShiftOut | ShiftIn| DataLinkEscape

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

    deriving (Eq, Ord, Enum, Bounded, Show, TH.Lift)


---  Direct usage of the Enum instance  ---

decodeCharIntUnsafe :: Int -> Char
decodeCharIntUnsafe = Enum.toEnum

encodeCharInt :: Char -> Int
encodeCharInt = Enum.fromEnum


---  Safe bound-checked usage of the Enum instance  ---

decodeCharInt :: Int -> Maybe Char

decodeCharInt x | x < 0   = May.Nothing
                | x > 127 = May.Nothing

decodeCharInt x = May.Just (decodeCharIntUnsafe x)


---  Character encoding  ---

-- Conversions between a 'Char' and its corresponding number between 0 and 127.
class CharEncoding a
  where
    {-# MINIMAL encodeChar, decodeChar #-}

    -- | Converts an ASCII 'Char' to its corresponding number between 0 and 127.
    encodeChar :: Char -> a

    -- | Converts a number between 0 and 127 to its corresponding ASCII 'Char'. Returns 'Nothing' for any numbers outside of this range.
    decodeChar :: a -> Maybe Char

    -- | Converts a number between 0 and 127 to its corresponding ASCII 'Char'. Returns the 'Substitute' character for any numbers outside of this range.
    decodeCharSub :: a -> Char
    decodeCharSub = substitute . decodeChar

instance CharEncoding Int
  where
    encodeChar = encodeCharInt
    decodeChar = decodeCharInt

instance CharEncoding Word8
  where
    encodeChar = Num.fromIntegral . encodeCharInt
    decodeChar = decodeCharInt . Num.fromIntegral

substitute :: Maybe Char -> Char
substitute = May.fromMaybe Substitute


---  Strings  --

newtype String = String { stringArray :: Array.UArray Int Word8 }
    deriving (Eq, Ord)

instance Show String
  where
    showsPrec _ str = Show.showString "[ascii|" . Show.showString (toUnicode str) . Show.showString "|]"

pack :: [Char] -> String
pack = String . word8ListArray . List.map encodeChar

word8ListArray :: [Word8] -> Array.UArray Int Word8
word8ListArray xs = Array.listArray (1, List.length xs) xs

unpack :: String -> [Char]
unpack = List.map (decodeCharIntUnsafe . Num.fromIntegral) . Array.elems . stringArray


---  Case  ---

data Case = UpperCase | LowerCase
    deriving (Eq, Ord, Enum, Bounded, Show)

isCase :: Case -> Char -> Bool
isCase UpperCase x = (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterZ)
isCase LowerCase x = (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterZ)

letterCase :: Char -> Maybe Case
letterCase x | isCase UpperCase x = May.Just UpperCase
             | isCase LowerCase x = May.Just LowerCase
letterCase _ = May.Nothing


---  Group  ---

data Group = Control | Printable
    deriving (Eq, Ord, Enum, Bounded, Show)

charGroup :: Char -> Group
charGroup x | (x < Space) = Control
charGroup Delete = Control
charGroup _ = Printable

inGroup :: Group -> Char -> Bool
inGroup g x = charGroup x == g


---  Case-insensitive equivalence  ---

class CaseInsensitiveEquivalence a
  where
    {-# MINIMAL caseInsensitiveEquivalence | equalsIgnoringCase #-}

    caseInsensitiveEquivalence :: Eq.Equivalence a
    caseInsensitiveEquivalence = Eq.Equivalence equalsIgnoringCase

    equalsIgnoringCase :: a -> a -> Bool
    equalsIgnoringCase = Eq.getEquivalence caseInsensitiveEquivalence

instance CaseInsensitiveEquivalence Char
  where
    caseInsensitiveEquivalence = Contra.contramap (toCase LowerCase) Eq.defaultEquivalence

instance CaseInsensitiveEquivalence String
  where
    caseInsensitiveEquivalence = Contra.contramap (List.map (toCase LowerCase) . unpack) Eq.defaultEquivalence


---  Case conversion  ---

class CaseConversion a
  where
    toCase :: Case -> a -> a

instance CaseConversion Char
  where
    toCase LowerCase x = if (isCase UpperCase x) then decodeCharIntUnsafe ((Num.+) (encodeCharInt x) 32) else x
    toCase UpperCase x = if (isCase LowerCase x) then decodeCharIntUnsafe ((Num.-) (encodeCharInt x) 32) else x

instance CaseConversion String
  where
    toCase c = pack . List.map (toCase c) . unpack


---  Conversions with Unicode types  ---

-- | The 'Unicode' type family associates each of the ASCII types defined in this module to its Unicode counterpart in "Prelude". Use the methods of the 'UnicodeConversion' class to convert to and fro between ASCII and Unicode.
type family Unicode a = u | u -> a
  where
    Unicode Char = Unicode.Char
    Unicode String = Unicode.String

class UnicodeConversion a
  where
    -- | Widen an ASCII character or string to its corresponding Unicode type.
    toUnicode :: a -> Unicode a

    -- | Narrow a Unicode character or string to ASCII, returning 'Nothing' if the input contains values that are not included in the ASCII character set.
    fromUnicodeMaybe :: Unicode a -> Maybe a

    -- | Narrow a Unicode character or string to ASCII, using the 'Substitute' character in place of any characters in the input that are not included in the ASCII character set.
    fromUnicodeSub :: Unicode a -> a

instance UnicodeConversion Char
  where
    toUnicode = Unicode.chr . encodeCharInt
    fromUnicodeSub = decodeCharSub . Unicode.ord
    fromUnicodeMaybe = decodeChar . Unicode.ord

instance UnicodeConversion String
  where
    toUnicode = List.map toUnicode . unpack
    fromUnicodeSub = pack . List.map fromUnicodeSub
    fromUnicodeMaybe = fmap pack . traverse fromUnicodeMaybe
