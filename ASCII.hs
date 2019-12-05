{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveLift #-}
    -- For the deriving clause on the 'Char' type

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
    -- For writing type annotations in a pattern context

{-# LANGUAGE TemplateHaskell #-}
    -- For defining the quasi-quoters

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}

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
    Char ( .. ), allCharacters

  -- * Strings
  , String, GenericString

  -- * String-[Char] conversions
  , pack, unpack

  -- * Quasi-quoters
  , char, list, string, bytes

  -- * Character groups
  , Group ( .. ), charGroup, inGroup, isControl, isPrint, controlCodes, printableCharacters

  -- * Upper/lower case letters
  , Case ( UpperCase, LowerCase ), isCase, isLower, isUpper, isAlpha, isLetter, letterCase, letters, capitalLetters, smallLetters, CaseInsensitiveEquivalence ( caseInsensitiveEquivalence, equalsIgnoringCase ), CaseConversion ( toCase )

  -- * ASCII swimming in larger worlds
  , CharWidening ( fromChar, toCharMaybe, toCharSub ), StringWidening ( fromString, toStringMaybe, toStringSub )

  -- * Numeric characters
  , isDigit, isOctDigit, isHexDigit, isNumber
  , digits, octDigits, hexDigits, numbers

  -- * Miscellaneous character classifications
  , isSpace, isAlphaNum, isMark, isPunctuation, isSymbol, isSeparator

  -- * String functions
  , empty, singleton, null, length, replicate, take, drop, span, reverse, any, all, append, concat, cons, snoc, uncons

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
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

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
-- This type has 128 nullary constructors, listed in order according to each character's 7-bit numeric code. Its derived 'Enum' instance can therefore be used to convert a 'Char' to its ASCII code and vice versa; but since 'Enum.toEnum' is a partial function, we recommend instead using 'fromChar', which cannot produce runtime errors.

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

allCharacters :: [Char]
allCharacters = Enum.enumFromTo minBound maxBound

intToCharUnsafe :: Int -> Char
intToCharUnsafe = Enum.toEnum

charToInt :: Char -> Int
charToInt = Enum.fromEnum


---  Safe bound-checked usage of the Enum instance  ---

intToCharMaybe :: Int -> Maybe Char

intToCharMaybe x | x < 0   = May.Nothing
                 | x > 127 = May.Nothing

intToCharMaybe x = May.Just (intToCharUnsafe x)



---  Character widening  ---

-- Conversions between a 'Char' and some other larger type.
--
-- Instances should follow these rules:
--
-- - @toCharMaybe (fromChar c)@ = @Just c@
-- - If @toCharMaybe x@ = @Just c@, then @toCharSub x@ = @c@
-- - If @toCharMaybe x@ = @Nothing@, then @toCharSub x@ = 'Substitute'

class CharWidening a
  where
    {-# MINIMAL fromChar, toCharMaybe #-}

    fromChar :: Char -> a

    -- | Returns 'Nothing' for any value that does not represent an ASCII character.
    toCharMaybe :: a -> Maybe Char

    -- | Returns the 'Substitute' character for any value that does not represent an ASCII character.
    toCharSub :: a -> Char
    toCharSub = May.fromMaybe Substitute . toCharMaybe

-- | Representation of an ASCII 'Char' as an 'Int' between 0 and 127.
instance CharWidening Int
  where
    fromChar = charToInt
    toCharMaybe = intToCharMaybe

-- | Representation of an ASCII 'Char' as a byte where the first bit is always 0.
instance CharWidening Word8
  where
    fromChar = charToWord8
    toCharMaybe = word8ToCharMaybe

-- | Representation of an ASCII 'Char' as one of the first 128 Unicode 'Unicode.Char's.
instance CharWidening Unicode.Char
  where
    fromChar = Unicode.chr . charToInt
    toCharMaybe = intToCharMaybe . Unicode.ord


---  Word8 Char conversions  ---

charToWord8 :: Char -> Word8
charToWord8 = Num.fromIntegral . charToInt

word8ToCharMaybe :: Word8 -> Maybe Char
word8ToCharMaybe = intToCharMaybe . Num.fromIntegral

word8ToCharUnsafe :: Word8 -> Char
word8ToCharUnsafe = intToCharUnsafe . Num.fromIntegral


---  Strings  --

-- | A strict sequence of ASCII 'Char's, packed as one byte per character.
--
-- We describe this as the "generic" string type because the @bytes@ type parameter specifies how the bytes are represented. The @bytes@ type should belong to the 'BA.ByteArray' class. The 'String' type alias represents our recommendation of 'BS.ByteString' as the standard choice for this type parameter.

newtype GenericString bytes = String { stringBytes :: bytes }
    deriving ( Eq, Ord )

-- | A strict sequence of ASCII 'Char's, packed as one byte per character.
--
-- The recommended way to write ASCII string literals is with the 'string' quasi-quoter.
--
-- Convert between a 'String' and a list of @'Char'@s with the 'pack' and 'unpack' functions.

type String = GenericString BS.ByteString

instance (BA.ByteArray bytes) => Show (GenericString bytes)
  where
    showsPrec d str = Show.showParen (d > 10) (Show.showString "ASCII.toStringSub " . Show.showsPrec 11 (fromString @Unicode.String str))

pack :: (BA.ByteArray bytes) => [Char] -> GenericString bytes
pack = String . BA.pack . List.map fromChar

unpack :: (BA.ByteArrayAccess bytes) => GenericString bytes -> [Char]
unpack = List.map (intToCharUnsafe . Num.fromIntegral) . BA.unpack . stringBytes


---  String widening  ---

-- | The 'GenericString' equivalent of 'CharWidening'.
--
-- Instances should follow the following rules:
--
-- - @toStringMaybe (fromString s)@ = @Just s@
-- - If @toStringMaybe x@ = @Just s@, then @toStringSub x@ = @s@

class StringWidening a
  where
    fromString :: (BA.ByteArrayAccess bytes) => GenericString bytes -> a
    toStringMaybe :: (BA.ByteArray bytes) => a -> Maybe (GenericString bytes)
    toStringSub :: (BA.ByteArray bytes) => a -> GenericString bytes

instance StringWidening [Unicode.Char]
  where
    fromString = List.map fromChar . unpack
    toStringMaybe = fmap pack . traverse @[] (toCharMaybe @Unicode.Char)
    toStringSub = pack . List.map (toCharSub @Unicode.Char)


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

instance (BA.ByteArrayAccess bytes) => CaseInsensitiveEquivalence (GenericString bytes)
  where
    caseInsensitiveEquivalence = Contra.contramap (List.map (toCase LowerCase) . unpack) Eq.defaultEquivalence


---  Case conversion  ---

class CaseConversion a
  where
    toCase :: Case -> a -> a

instance CaseConversion Char
  where
    toCase LowerCase x = if (isCase UpperCase x) then intToCharUnsafe ((Num.+) (charToInt x) 32) else x
    toCase UpperCase x = if (isCase LowerCase x) then intToCharUnsafe ((Num.-) (charToInt x) 32) else x

instance (BA.ByteArray bytes) => CaseConversion (GenericString bytes)
  where
    toCase c = pack . List.map (toCase c) . unpack


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


---  String functions  ---

-- | The empty string.
empty :: (BA.ByteArray bytes) => GenericString bytes
empty = String BA.empty

-- | A string consisting of a single character.
singleton :: (BA.ByteArray bytes) => Char -> GenericString bytes
singleton = String . BA.singleton . charToWord8

-- | Determine whether a string is empty.
null :: BA.ByteArrayAccess bytes => GenericString bytes -> Bool
null = BA.null . stringBytes

-- | The number of characters in a string.
length :: BA.ByteArrayAccess bytes => GenericString bytes -> Int
length = BA.length . stringBytes

-- | Analogous to 'List.replicate' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.replicate 5 ASCII.CapitalLetterA@ = @[ASCII.string|AAAAA|]@

replicate :: (BA.ByteArray bytes) => Int -> Char -> GenericString bytes
replicate n = String . BA.replicate n . charToWord8

-- | Analogous to 'List.take' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.take 3 [ASCII.string|December|]@ = @[ASCII.string|Dec|]@

take :: (BA.ByteArray bytes) => Int -> GenericString bytes -> GenericString bytes
take n = String . BA.take n . stringBytes

-- | Analogous to 'List.drop' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.drop 3 [ASCII.string|December|]@ = @[ASCII.string|ember|]@

drop :: (BA.ByteArray bytes) => Int -> GenericString bytes -> GenericString bytes
drop n = String . BA.drop n . stringBytes

-- | Analogous to 'List.span' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.span ASCII.isLower [ASCII.string|oneTWOthree|]@ = @([ASCII.string|one|], [ASCII.string|TWOthree|])@

span :: (BA.ByteArray bytes) => (Char -> Bool) -> GenericString bytes -> (GenericString bytes, GenericString bytes)
span f = (\(x, y) -> (String x, String y)) . BA.span (f . word8ToCharUnsafe) . stringBytes

-- | Analogous to 'List.reverse' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.reverse [ASCII.string|fish|]@ = @[ASCII.string|hsif|]@

reverse :: (BA.ByteArray bytes) => GenericString bytes -> GenericString bytes
reverse = String . BA.reverse . stringBytes

-- | Analogous to 'List.any' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.any ASCII.isDigit [ASCII.string|fish|]@ = @False@
--
-- @ASCII.any ASCII.isDigit [ASCII.string|fish2|]@ = @True@

any :: (BA.ByteArrayAccess bytes) => (Char -> Bool) -> GenericString bytes -> Bool
any f = BA.any (f . word8ToCharUnsafe) . stringBytes

-- | Analogous to 'List.all' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.all ASCII.isLetter [ASCII.string|fish|]@ = @True@
--
-- @ASCII.all ASCII.isLetter [ASCII.string|fish2|]@ = @False@

all :: (BA.ByteArrayAccess bytes) => (Char -> Bool) -> GenericString bytes -> Bool
all f = BA.all (f . word8ToCharUnsafe) . stringBytes

-- | Concatenation of two strings.
--
-- ==== Examples
--
-- @ASCII.append [ASCII.string|One|] [ASCII.string|Two|]@ = @[ASCII.string|OneTwo|]@

append :: (BA.ByteArray bytes) => GenericString bytes -> GenericString bytes -> GenericString bytes
append x y = String (BA.append (stringBytes x) (stringBytes y))

-- | Concatenation of a list of strings.
--
-- ==== Examples
--
-- @ASCII.concat [ [ASCII.string|One|], [ASCII.string|Two|], [ASCII.string|Three|] ]@ = @[ASCII.string|OneTwoThree|]@

concat :: (BA.ByteArrayAccess inputBytes, BA.ByteArray outputBytes) => [GenericString inputBytes] -> GenericString outputBytes
concat = String . BA.concat . List.map stringBytes

-- | Prepend a character to the beginning of a string, analogous to the @(:)@ operator which is also sometimes called "cons".
--
-- ==== Examples
--
-- @ASCII.cons ASCII.CapitalLetterC [ASCII.string|at|]@ = @[ASCII.string|Cat|]@

cons :: (BA.ByteArray bytes) => Char -> GenericString bytes -> GenericString bytes
cons x xs = String (BA.cons (charToWord8 x) (stringBytes xs))

-- | Appends a character to the end of a string. Because this function is similar to 'cons' but in the reverse direction, its name is "cons" spelled backwards.
--
-- ==== Examples
--
-- @ASCII.snoc [ASCII.string|Ca|] ASCII.SmallLetterT@ = @[ASCII.string|Cat|]@

snoc :: (BA.ByteArray bytes) => GenericString bytes -> Char -> GenericString bytes
snoc xs x = String (BA.snoc (stringBytes xs) (charToWord8 x))

-- | Analogous to 'List.uncons' from the "Data.List" module.
--
-- ==== Examples
--
-- @ASCII.uncons [ASCII.string||]@ = @Nothing@
--
-- @ASCII.uncons [ASCII.string|Cat|]@ = @Just (ASCII.CapitalLetterC, [ASCII.string|at|])@

uncons :: (BA.ByteArray bytes) => GenericString bytes -> Maybe (Char, GenericString bytes)
uncons = fmap (\(x, xs) -> (word8ToCharUnsafe x, String xs)) . BA.uncons . stringBytes


---  Quasi-quotation  ---

failQ :: Unicode.String -> Q a
failQ = MonadFail.fail

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
    requireAscii = (May.maybe notAscii return) . (toCharMaybe @Unicode.Char)

    wrongContext = failQ "The ASCII.char quasi-quoter may only be used in an expression or pattern context."
    tooShort = failQ "The ASCII.char quasi-quoter cannot be empty; it must contain a character."
    tooLong = failQ "The ASCII.char quasi-quoter may only be given a single character."
    notAscii = failQ "The ASCII.char quasi-quoter only works with an ASCII character."

charPat :: Char -> Q TH.Pat
charPat c = TH.ConP <$> lookupConName <*> return []
  where
    conName = G.conNameOf c
    lookupConName = TH.lookupValueName (List.concat ["ASCII.", conName]) >>= May.maybe lookupFailed return
    lookupFailed = failQ (List.concat ["TH.lookupValueName \"ASCII.", conName, "\" failed. This means there is a mistake in the ascii library."])

{- | Produces an expression representing a 'String' value corresponding to the quasiquoted string.

> >>> :set -XQuasiQuotes
>
> >>> import qualified ASCII
>
> >>> [ASCII.string|Hi!|]
> ASCII.toStringSub "Hi!"

The expression fails to compile if the quasiquoted string contains characters that cannot be represented in the ASCII character set.

> >>> [ASCII.string|helloλ|]
>
> interactive>:4:8: error:
>     • The ascii quasi-quoter cannot be used with non-ASCII characters.
>     • In the quasi-quotation: [ascii|helloλ|]

-}

string :: QQ.QuasiQuoter
string =
  QQ.QuasiQuoter
    { QQ.quoteExp = requireAscii >=> \x -> [e|ASCII.pack $(TH.lift (ASCII.unpack x)) :: ASCII.String|]
    , QQ.quotePat = \_ -> wrongContext
    , QQ.quoteType = \_ -> wrongContext
    , QQ.quoteDec = \_ -> wrongContext
    }
  where
    requireAscii :: Unicode.String -> Q String
    requireAscii = (May.maybe notAscii return) . (toStringMaybe @Unicode.String)

    wrongContext = failQ "The ASCII.string quasi-quoter may only be used in an expression context."
    notAscii = failQ "The ASCII.string quasi-quoter only works with ASCII characters."

bytes :: QQ.QuasiQuoter
bytes =
  QQ.QuasiQuoter
    { QQ.quoteExp = requireAscii >=> \x -> [e|ASCII.pack $(TH.lift (ASCII.unpack x)) :: GenericString BA.Bytes|]
    , QQ.quotePat = \_ -> wrongContext
    , QQ.quoteType = \_ -> wrongContext
    , QQ.quoteDec = \_ -> wrongContext
    }
  where
    requireAscii :: Unicode.String -> Q (GenericString BA.Bytes)
    requireAscii = (May.maybe notAscii return) . (toStringMaybe @Unicode.String)

    wrongContext = failQ "The ASCII.bytes quasi-quoter may only be used in an expression context."
    notAscii = failQ "The ASCII.bytes quasi-quoter only works with ASCII characters."

list :: QQ.QuasiQuoter
list =
  QQ.QuasiQuoter
    { QQ.quoteExp = requireAscii >=> TH.lift @[Char]
    , QQ.quotePat = \_ -> wrongContext
    , QQ.quoteType = \_ -> wrongContext
    , QQ.quoteDec = \_ -> wrongContext
    }
  where
    requireAscii :: Unicode.String -> Q [Char]
    requireAscii = (May.maybe notAscii return) . traverse (toCharMaybe @Unicode.Char)

    wrongContext = failQ "The ASCII.list quasi-quoter may only be used in an expression context."
    notAscii = failQ "The ASCII.list quasi-quoter only works with ASCII characters."
