{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveLift #-}
    -- For the deriving clause on the 'Char' type

{-# LANGUAGE TypeFamilies #-}
    -- For the 'Unicode' type family

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
    -- For writing type annotations in a pattern context

{-# LANGUAGE TemplateHaskell #-}
    -- For defining the quasi-quoters

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DeriveGeneric #-}

{- |

The __American Standard Code for Information Interchange__ (ASCII) comprises a set of 128 characters, each represented by 7 bits. 33 of these characters are /'Control' codes/; a few of these are still in use, but most are obsolete relics of the early days of computing. The other 95 are /'Printable' characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as UTF-8 that can represent the entire Unicode character set, which includes over a hundred thousand characters and is not limited to the symbols of any particular writing system or culture. However, ASCII is still relevant to network protocols; for example, we can see it in the specification of [HTTP message headers](https://tools.ietf.org/html/rfc7230#section-1.2).

There is a convenient relationship between ASCII and Unicode: the ASCII characters are the first 128 characters of the much larger Unicode character set. The [C0 Controls and Basic Latin](https://www.unicode.org/charts/PDF/U0000.pdf) section of the Unicode standard contains a list of all the ASCII characters. You may also find this list replicated below; each ASCII character corresponds to a constructor of the 'Char' type.

We do not elaborate on the semantics of the control characters here, because this information is both obsolete and restricted by copyright law. It is described by a document entitled /"Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)"/, published by American National Standards Institute (ANSI) and available for purchase [on their website](https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012).

== Recommended import style

> import qualified ASCII

== Relationship to @Data.Char@

The following are drop-in replacements for closely related definitions of the same name in the "Data.Char" module: 'isControl', 'isSpace', 'isLower', 'isUpper', 'isAlpha', 'isAlphaNum', 'isPrint', 'isDigit', 'isOctDigit', 'isHexDigit', 'isLetter', 'isMark', 'isNumber', 'isPunctuation', 'isSymbol', 'isSeparator'.

-}

module ASCII
  (

  -- * Characters
    Char (..), all

  -- * Char encoding
  , CharEncoding (..)

  -- * Strings
  , String

  -- * String-[Char] conversions
  , pack, unpack

  -- * Quasi-quoters
  , char
  -- todo, list, string
  , bytes
  -- todo, bytestring

  -- * Character groups
  , Group ( .. ), charGroup, inGroup, isControl, isPrint, controlCodes, printableCharacters

  -- * Upper/lower case letters
  , Case (..), isCase, isLower, isUpper, isAlpha, isLetter, letterCase, letters, capitalLetters, smallLetters, CaseInsensitiveEquivalence ( .. ), CaseConversion ( .. )

  -- * Unicode conversion
  , Unicode, UnicodeConversion ( .. )

  -- * Numeric characters
  , isDigit, isOctDigit, isHexDigit, isNumber
  , digits, octDigits, hexDigits, numbers

  -- * Miscellaneous character classifications
  , isSpace, isAlphaNum, isMark, isPunctuation, isSymbol, isSeparator

  ) where

import Prelude ( Ord (..), fmap )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( (>>=), (>=>), return )
import Data.Function ( (.) )
import Data.Eq ( Eq ( (==) ) )
import qualified Data.List as List

-- Min and max bounds
import Prelude ( Bounded ( minBound, maxBound ) )

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
import qualified Data.Maybe as May ( Maybe ( .. ), fromMaybe, maybe )

-- Tightly-packed sequences of bytes.
import qualified Data.ByteArray as Bytes

-- For defining the expression-context quasi-quoters.
import Language.Haskell.TH.Syntax ( Q )
import qualified Language.Haskell.TH.Quote as QQ ( QuasiQuoter (..) )
import qualified Language.Haskell.TH.Syntax as TH ( Lift ( lift ) )
import qualified Control.Monad.Fail as MonadFail

-- For defining the pattern-context quasi-quoter.
import qualified Language.Haskell.TH.Syntax as TH ( Pat ( ConP ), lookupValueName )

-- Generics
import qualified GHC.Generics as G
import qualified Generics.Deriving.ConNames as G


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

    deriving ( Eq, Ord, Enum, Bounded, Show )

-- Requires the DeriveLift language extension.
deriving instance TH.Lift Char

-- Requires the DeriveGeneric language extension.
deriving instance G.Generic Char


---  Direct usage of the Enum instance  ---

all :: [Char]
all = Enum.enumFromTo minBound maxBound

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
    decodeCharSub = May.fromMaybe Substitute . decodeChar

instance CharEncoding Int
  where
    encodeChar = encodeCharInt
    decodeChar = decodeCharInt

instance CharEncoding Word8
  where
    encodeChar = Num.fromIntegral . encodeCharInt
    decodeChar = decodeCharInt . Num.fromIntegral


---  Strings  --

-- | A strict sequence of ASCII 'Char's, packed as one byte per character.
--
-- Convert between String and @['Char']@ with the 'pack' and 'unpack' functions.
--
-- The recommended way to write ASCII string literals is with the 'ASCII.QQ.ascii' quasi-quoter in the "ASCII.QQ" module.

newtype String bytes = String { stringBytes :: bytes }
    deriving ( Eq, Ord )

instance (Bytes.ByteArray bytes) => Show (String bytes)
  where
    showsPrec d str = Show.showParen (d > 10) (Show.showString "ASCII.fromUnicodeSub " . Show.showsPrec 11 (toUnicode str))

pack :: (Bytes.ByteArray bytes) => [Char] -> String bytes
pack = String . Bytes.pack . List.map encodeChar

unpack :: (Bytes.ByteArrayAccess bytes) => String bytes -> [Char]
unpack = List.map (decodeCharIntUnsafe . Num.fromIntegral) . Bytes.unpack . stringBytes


---  Case  ---

data Case = UpperCase | LowerCase
    deriving ( Eq, Ord, Enum, Bounded, Show )

isCase :: Case -> Char -> Bool
isCase UpperCase x = (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterZ)
isCase LowerCase x = (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterZ)

letterCase :: Char -> Maybe Case
letterCase x | isCase UpperCase x = May.Just UpperCase
             | isCase LowerCase x = May.Just LowerCase
letterCase _ = May.Nothing

-- | Returns True for 'LowerCase' letters, from 'SmallLetterA' to 'SmallLetterZ'.
--
-- This function is analogous to 'Unicode.isLower' in the "Data.Char" module.

isLower :: Char -> Bool
isLower = isCase LowerCase

-- | Returns True for 'UpperCase' letters, from 'CapitalLetterA' to 'CapitalLetterZ'.
--
-- This function is analogous to 'Unicode.isUpper' in the "Data.Char" module.

isUpper :: Char -> Bool
isUpper = isCase UpperCase

-- | Returns True for letters:
--
-- - 'SmallLetterA' to 'SmallLetterZ'
-- - 'CapitalLetterA' to 'CapitalLetterZ'
--
-- This function is analogous to 'Unicode.isLetter' in the "Data.Char" module.

isLetter :: Char -> Bool
isLetter x = (Bool.||) (isLower x) (isUpper x)

-- | Synonym for 'isLetter'.
--
-- This function is analogous to 'Unicode.isAlpha' in the "Data.Char" module.

isAlpha :: Char -> Bool
isAlpha = isLetter

letters :: [Char]
letters = (List.++) capitalLetters smallLetters

capitalLetters :: [Char]
capitalLetters = Enum.enumFromTo CapitalLetterA CapitalLetterZ

smallLetters :: [Char]
smallLetters = Enum.enumFromTo SmallLetterA SmallLetterZ


---  Group  ---

data Group = Control | Printable
    deriving ( Eq, Ord, Enum, Bounded, Show )

charGroup :: Char -> Group
charGroup x | (x < Space) = Control
charGroup Delete = Control
charGroup _ = Printable

inGroup :: Group -> Char -> Bool
inGroup g x = charGroup x == g

-- | A list of all characters in the 'Control' group.
controlCodes :: [Char]
controlCodes = (List.++) (Enum.enumFromTo Null UnitSeparator) [Delete]

-- | A list of all characters in the 'Printable' group.
printableCharacters :: [Char]
printableCharacters = Enum.enumFromTo Space Tilde

-- | Returns True for characters in the 'Control' group.
--
-- This function is analogous to 'Unicode.isControl' in the "Data.Char" module.

isControl :: Char -> Bool
isControl = inGroup Control

-- | Returns True for characters in the 'Printable' group.
--
-- This function is analogous to 'Unicode.isPrint' in the "Data.Char" module.

isPrint :: Char -> Bool
isPrint = inGroup Printable


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

instance (Bytes.ByteArrayAccess bytes) => CaseInsensitiveEquivalence (String bytes)
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

instance (Bytes.ByteArray bytes) => CaseConversion (String bytes)
  where
    toCase c = pack . List.map (toCase c) . unpack


---  Conversions with Unicode types  ---

-- | The 'Unicode' type family associates each of the ASCII types defined in this module to its Unicode counterpart in "Prelude". Use the methods of the 'UnicodeConversion' class to convert to and fro between ASCII and Unicode.

type family Unicode a = u
  where
    Unicode Char = Unicode.Char
    Unicode (String bytes) = Unicode.String

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

instance (Bytes.ByteArray bytes) => UnicodeConversion (String bytes)
  where
    toUnicode = List.map toUnicode . unpack
    fromUnicodeSub = pack . List.map fromUnicodeSub
    fromUnicodeMaybe = fmap pack . traverse fromUnicodeMaybe


---  Numeric characters  ---

-- | Returns True for the characters from 'Digit0' to 'Digit9'.
--
-- This function is analogous to 'Unicode.isDigit' in the "Data.Char" module.

isDigit :: Char -> Bool
isDigit x = (Bool.&&) (x >= Digit0) (x <= Digit9)

digits :: [Char]
digits = Enum.enumFromTo Digit0 Digit9

-- | Returns True for the characters from 'Digit0' to 'Digit7'.
--
-- This function is analogous to 'Unicode.isOctDigit' in the "Data.Char" module.
isOctDigit :: Char -> Bool
isOctDigit x = (Bool.&&) (x >= Digit0) (x <= Digit7)

octDigits :: [Char]
octDigits = Enum.enumFromTo Digit0 Digit7

-- | Returns True for characters in any of the following ranges:
--
-- - 'Digit0' to 'Digit9'
-- - 'CapitalLetterA' to 'CapitalLetterF'
-- - 'SmallLetterA' to 'SmallLetterF'
--
-- This function is analogous to 'Unicode.isHexDigit' in the "Data.Char" module.

isHexDigit :: Char -> Bool
isHexDigit x | isDigit x = True
             | (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterF) = True
             | (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterF) = True
isHexDigit _ = False

hexDigits :: [Char]
hexDigits =
  List.concat
    [ digits
    , Enum.enumFromTo CapitalLetterA CapitalLetterF
    , Enum.enumFromTo SmallLetterA SmallLetterF
    ]

-- | Synonym for 'isDigit'.
--
-- In the "Data.Char" module, 'Unicode.isDigit' selects only the ASCII digits 0 through 9, and 'Unicode.isNumber' selects a wider set of characters because the full Unicode character set contains more numeric characters than just the ASCII digits. In this module, these two functions are redundant, but we include this synonym for compatibility with "Data.Char".

isNumber :: Char -> Bool
isNumber = isDigit

-- | Synonym for 'digits'.
numbers :: [Char]
numbers = digits


---  Character classification  ---

-- | Returns True for the following characters:
--
-- - 'Space'
-- - 'HorizontalTab'
-- - 'LineFeed'
-- - 'VerticalTab'
-- - 'FormFeed'
-- - 'CarriageReturn'
--
-- This function is analogous to 'Unicode.isSpace' in the "Data.Char" module.

isSpace :: Char -> Bool
isSpace Space = True
isSpace x = (Bool.&&) (x >= HorizontalTab) (x <= CarriageReturn)

-- | This function is analogous to 'Unicode.isAlphaNum' in the "Data.Char" module.
isAlphaNum :: Char -> Bool
isAlphaNum x = (Bool.||) (isAlpha x) (isDigit x)

-- | Selects mark characters, for example accents and the like, which combine with preceding characters. This always returns False because ASCII does not include any mark characters. This function is included only for compatibility with 'Unicode.isMark' in the "Data.Char" module.

isMark :: Char -> Bool
isMark _ = False

-- | Returns True for the following characters:
--
-- - 'ExclamationMark'
-- - 'QuotationMark'
-- - 'NumberSign'
-- - 'PercentSign'
-- - 'Ampersand'
-- - 'Apostrophe'
-- - 'LeftParenthesis'
-- - 'RightParenthesis'
-- - 'Asterisk'
-- - 'Comma'
-- - 'HyphenMinus'
-- - 'FullStop'
-- - 'Slash'
-- - 'Colon'
-- - 'Semicolon'
-- - 'QuestionMark'
-- - 'AtSign'
-- - 'LeftSquareBracket'
-- - 'Backslash'
-- - 'RightSquareBracket'
-- - 'Underscore'
-- - 'LeftCurlyBracket'
-- - 'RightCurlyBracket'
--
-- This function is analogous to 'Unicode.isPunctuation' in the "Data.Char" module.

isPunctuation :: Char -> Bool
isPunctuation = (`List.elem` [ExclamationMark, QuotationMark, NumberSign, PercentSign, Ampersand, Apostrophe, LeftParenthesis, RightParenthesis, Asterisk, Comma, HyphenMinus, FullStop, Slash, Colon, Semicolon, QuestionMark, AtSign, LeftSquareBracket, Backslash, RightSquareBracket, Underscore, LeftCurlyBracket, RightCurlyBracket])

-- | Returns True for the following characters:
--
-- - 'DollarSign'
-- - 'PlusSign'
-- - 'LessThanSign'
-- - 'EqualsSign'
-- - 'GreaterThanSign'
-- - 'Caret'
-- - 'GraveAccent'
-- - 'VerticalLine'
-- - 'Tilde'
--
-- This function is analogous to 'Unicode.isSymbol' in the "Data.Char" module.

isSymbol :: Char -> Bool
isSymbol = (`List.elem` [DollarSign, PlusSign, LessThanSign, EqualsSign, GreaterThanSign, Caret, GraveAccent, VerticalLine, Tilde])

-- | Returns True if the character is 'Space'.
--
-- This function is analogous to 'Unicode.isSeparator' in the "Data.Char" module.

isSeparator :: Char -> Bool
isSeparator = (== Space)


---  Quasi-quotation  --

failQ :: Unicode.String -> Q a
failQ = MonadFail.fail

charPat :: Char -> Q TH.Pat
charPat c = TH.ConP <$> lookupConName <*> return []
  where
    conName = G.conNameOf c
    lookupConName = TH.lookupValueName (List.concat ["ASCII.", conName]) >>= May.maybe lookupFailed return
    lookupFailed = failQ (List.concat ["TH.lookupValueName \"ASCII.", conName, "\" failed. This means there is a mistake in the ascii library."])

char :: QQ.QuasiQuoter
char =
  QQ.QuasiQuoter
    { QQ.quoteExp = requireOne >=> requireAscii >=> TH.lift @Char
    , QQ.quotePat = requireOne >=> requireAscii >=> charPat
    , QQ.quoteType = \_ -> wrongContext
    , QQ.quoteDec = \_ -> wrongContext
    }
  where
    requireOne :: Unicode.String -> Q Unicode.Char
    requireOne str = case str of [] -> tooShort; [x] -> return x; _ -> tooLong

    requireAscii :: Unicode.Char -> Q Char
    requireAscii = May.maybe notAscii return . fromUnicodeMaybe

    wrongContext = failQ "The ASCII.char quasi-quoter may only be used in an expression or pattern context."
    tooShort = failQ "The ASCII.char quasi-quoter cannot be empty; it must contain a character."
    tooLong = failQ "The ASCII.char quasi-quoter may only be given a single character."
    notAscii = failQ "The ASCII.char quasi-quoter only works with an ASCII character."

bytes :: QQ.QuasiQuoter
bytes =
  QQ.QuasiQuoter
    { QQ.quoteExp = requireAscii >=> \x -> [e|ASCII.pack $(TH.lift (ASCII.unpack x)) :: ASCII.String Bytes.Bytes|]
    , QQ.quotePat = \_ -> wrongContext
    , QQ.quoteType = \_ -> wrongContext
    , QQ.quoteDec = \_ -> wrongContext
    }
  where
    requireAscii :: Unicode.String -> Q (String Bytes.Bytes)
    requireAscii = May.maybe notAscii return . fromUnicodeMaybe

    wrongContext = failQ "The ASCII.bytes quasi-quoter may only be used in an expression context."
    notAscii = failQ "The ASCII.bytes quasi-quoter only works with ASCII characters."

-- todo
-- list :: QQ.QuasiQuoter

{- | Produces an expression representing a 'ASCII.String' value corresponding to the quasiquoted string.

-- todo: update these examples

> >>> :set -XQuasiQuotes
>
> >>> import qualified ASCII
>
> >>> import ASCII.QQ
>
> >>> ASCII.unpack [ASCII.string|Hi!|]
> [CapitalLetterH,SmallLetterI,ExclamationMark]

The expression fails to compile if the quasiquoted string contains characters that cannot be represented in the ASCII character set.

> >>> [ASCII.string|helloλ|]
>
> interactive>:4:8: error:
>     • The ascii quasi-quoter cannot be used with non-ASCII characters.
>     • In the quasi-quotation: [ascii|helloλ|]

-}

-- string :: QQ.QuasiQuoter
-- string =
--   QQ.QuasiQuoter
--     { QQ.quoteExp  = \str ->
--           case ASCII.fromUnicodeMaybe str of
--               May.Nothing -> failQ "The ASCII.string quasi-quoter cannot be used with non-ASCII characters."
--               May.Just (x :: ASCII.String Bytes.Bytes) -> lift x
--     , QQ.quotePat  = wrongContext
--     , QQ.quoteType = wrongContext
--     , QQ.quoteDec  = wrongContext
--     }
--   where
--     wrongContext :: Unicode.String -> Q a
--     wrongContext _ = failQ "The ASCII.string quasi-quoter may only be used in an expression context."

--     failQ :: Unicode.String -> Q a
--     failQ = MonadFail.fail
