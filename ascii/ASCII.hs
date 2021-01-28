{- |

The __American Standard Code for Information Interchange__ (ASCII) comprises a set of 128 characters, each represented by 7 bits. 33 of these characters are /'Control' codes/; a few of these are still in use, but most are obsolete relics of the early days of computing. The other 95 are /'Printable' characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as UTF-8 that can represent the entire Unicode character set, which includes over a hundred thousand characters and is not limited to the symbols of any particular writing system or culture. However, ASCII is still relevant to network protocols; for example, we can see it in the specification of [HTTP message headers](https://tools.ietf.org/html/rfc7230#section-1.2).

There is a convenient relationship between ASCII and Unicode: the ASCII characters are the first 128 characters of the much larger Unicode character set. The [C0 Controls and Basic Latin](https://www.unicode.org/charts/PDF/U0000.pdf) section of the Unicode standard contains a list of all the ASCII characters. You may also find this list replicated in the "ASCII.Char" module; each ASCII character corresponds to a constructor of the 'ASCII.Char' type.

We do not elaborate on the semantics of the control characters here, because this information is both obsolete and restricted by copyright law. It is described by a document entitled /"Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)"/, published by American National Standards Institute (ANSI) and available for purchase [on their website](https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012).

-}

module ASCII
  (
    {- * @Char@ -} {- $char -} Char,

    {- * Character classifications -}
    {- ** Print/control groups -} {- $groups -} Group (..), charGroup,  inGroup,
    {- ** Upper/lower case     -} {- $case   -} Case (..),  letterCase, isCase, toCaseChar, toCaseString,

    {- * Monomorphic conversions -} {- $monomorphicConversions -}
    {- ** @Int@        -} {- $intConversions           -} charToInt,               intToCharMaybe,               intToCharUnsafe,
    {- ** @Word8@      -} {- $word8Conversions         -} charToWord8,             word8ToCharMaybe,             word8ToCharUnsafe,
    {- ** @Char@       -} {- $unicodeCharConversions   -} charToUnicode,           unicodeToCharMaybe,           unicodeToCharUnsafe,
    {- ** @String@     -} {- $unicodeStringConversions -} charListToUnicodeString, unicodeStringToCharListMaybe, unicodeStringToCharListUnsafe,
    {- ** @Text@       -} {- $textConversions          -} charListToText,          textToCharListMaybe,          textToCharListUnsafe,
    {- ** @ByteString@ -} {- $byteStringConversions    -} charListToByteString,    byteStringToCharListMaybe,    byteStringToCharListUnsafe,

    {- * Monomorphic conversions between ASCII supersets -} {- $monoSupersetConversions -}
    {- ** @ByteString@ / @String@ -} byteStringToUnicodeStringMaybe, unicodeStringToByteStringMaybe,
    {- ** @[Word8]@      / @String@ -} byteListToUnicodeStringMaybe,   unicodeStringToByteListMaybe,

    {- * Refinement type -} {- $refinement -} {- ** @ASCII@ -} ASCII,

    {- * Polymorphic conversions -}
    {- ** Validate -} validateChar, validateString,
    {- ** Lift -} {- $lift -} lift,
    {- ** Convert -} {- $supersetConversions -} convertCharMaybe, convertCharOrFail, convertStringMaybe, convertStringOrFail,

    {- * Classes -} {- ** @CharSuperset@ -} CharSuperset, {- ** @StringSuperset@ -} StringSuperset, {- ** @Lift@ -} Lift, {- ** @CharIso@ -} CharIso, {- ** @StringIso@ -} StringIso,

    {- * Quasi-quoters -} {- ** @char@ -} char, {- ** @string@ -} string
  )
  where

import ASCII.Case          ( Case (..) )
import ASCII.Char          ( Char )
import ASCII.Group         ( Group (..) )
import ASCII.Isomorphism   ( CharIso, StringIso )
import ASCII.Lift          ( Lift )
import ASCII.QuasiQuoters  ( char, string )
import ASCII.Refinement    ( ASCII, validateChar, validateString )
import ASCII.Superset      ( CharSuperset, StringSuperset )

import Control.Monad       ( (>=>) )
import Control.Monad.Fail  ( MonadFail )
import Data.Bool           ( Bool (..) )
import Data.Function       ( (.) )
import Data.Int            ( Int )
import Data.Maybe          ( Maybe, maybe )
import Data.Word           ( Word8 )

import qualified  ASCII.Case
import qualified  ASCII.Group
import qualified  ASCII.Isomorphism
import qualified  ASCII.Lift
import qualified  ASCII.Superset

import qualified  Data.ByteString  as  BS
import qualified  Data.Char        as  Unicode
import qualified  Data.String      as  Unicode
import qualified  Data.Text        as  Text

{- $setup

>>> import ASCII.Char (Char (..))
>>> import Data.List (map)
>>> import Data.Text (Text)
>>> import Data.Word (Word8)

-}

{- $char

See also: "ASCII.Char"

-}

{- $groups

ASCII characters are broadly categorized into two groups: /control codes/ and /printable characters/.

See also: "ASCII.Group"

-}


{- | Determine which group a particular character belongs to.

>>> map charGroup [CapitalLetterA,EndOfTransmission]
[Printable,Control]

-}

charGroup :: CharIso char => char -> Group
charGroup = ASCII.Group.charGroup . ASCII.Isomorphism.toChar

{- | Test whether a character belongs to a particular ASCII group.

>>> inGroup Printable EndOfTransmission
False

>>> inGroup Control EndOfTransmission
True

>>> map (inGroup Printable) ( [-1, 5, 65, 97, 127, 130] :: [Int] )
[False,False,True,True,False,False]

-}

inGroup :: CharSuperset char => Group -> char -> Bool
inGroup g = maybe False (ASCII.Group.inGroup g) . ASCII.Superset.toCharMaybe

{- $case

/Case/ is a property of letters. /A-Z/ are /upper case/ letters, and /a-z/ are /lower case/ letters. No other ASCII characters have case.

See also: "ASCII.Case"

-}

{- | Determines whether a character is an ASCII letter, and if so, whether it is upper or lower case.

>>> map letterCase [SmallLetterA, CapitalLetterA, ExclamationMark]
[Just LowerCase,Just UpperCase,Nothing]

>>> map letterCase ( [string|Hey!|] :: [ASCII Word8] )
[Just UpperCase,Just LowerCase,Just LowerCase,Nothing]

-}

letterCase :: CharSuperset char => char -> Maybe Case
letterCase = ASCII.Superset.toCharMaybe >=> ASCII.Case.letterCase

{- | Determines whether a character is an ASCII letter of a particular case.

>>> map (isCase UpperCase) [SmallLetterA, CapitalLetterA, ExclamationMark]
[False,True,False]

>>> map (isCase UpperCase) ( [string|Hey!|] :: [ASCII Word8] )
[True,False,False,False]

>>> map (isCase UpperCase) ( [-1, 65, 97, 150] :: [Int] )
[False,True,False,False]

-}

isCase :: CharSuperset char => Case -> char -> Bool
isCase c = maybe False (ASCII.Case.isCase c) . ASCII.Superset.toCharMaybe

{- | Maps a letter character to its upper/lower case equivalent.

>>> toCaseChar UpperCase SmallLetterA
CapitalLetterA

>>> ([char|a|] :: ASCII Word8, toCaseChar UpperCase [char|a|] :: ASCII Word8)
(asciiUnsafe 97,asciiUnsafe 65)

-}

toCaseChar :: CharIso char => Case -> char -> char
toCaseChar c = ASCII.Isomorphism.asChar (ASCII.Case.toCase c)

{- | Maps each of the characters in a string to its upper/lower case equivalent.

>>> toCaseString UpperCase [CapitalLetterH,SmallLetterE,SmallLetterY,ExclamationMark]
[CapitalLetterH,CapitalLetterE,CapitalLetterY,ExclamationMark]

>>> toCaseString UpperCase [string|Hey!|] :: ASCII Text
asciiUnsafe "HEY!"

-}

toCaseString :: StringIso string => Case -> string -> string
toCaseString c = ASCII.Isomorphism.mapChars (ASCII.Case.toCase c)

{- $monomorphicConversions

These are a few simple monomorphic functions to convert between ASCII and types representing some other character set.

This is not intended to be an exhaustive list of all possible conversions. For more options, see "ASCII.Superset".

-}

{- $intConversions

These functions convert between the ASCII 'Char' type and 'Int'.

-}

{- |

>>> map charToInt [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

charToInt :: Char -> Int
charToInt = ASCII.Superset.fromChar

intToCharMaybe :: Int -> Maybe Char
intToCharMaybe = ASCII.Superset.toCharMaybe

intToCharUnsafe :: Int -> Char
intToCharUnsafe = ASCII.Superset.toCharUnsafe

{- $word8Conversions

These functions convert between the ASCII 'Char' type and 'Word8'.

-}

{- |

>>> map charToWord8 [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

charToWord8 :: Char -> Word8
charToWord8 = ASCII.Superset.fromChar

word8ToCharMaybe :: Word8 -> Maybe Char
word8ToCharMaybe = ASCII.Superset.toCharMaybe

word8ToCharUnsafe :: Word8 -> Char
word8ToCharUnsafe = ASCII.Superset.toCharUnsafe

{- $unicodeCharConversions

These functions convert between the ASCII 'Char' type and the Unicode 'Unicode.Char' type.

-}

charToUnicode :: Char -> Unicode.Char
charToUnicode = ASCII.Superset.fromChar

unicodeToCharMaybe :: Unicode.Char -> Maybe Char
unicodeToCharMaybe = ASCII.Superset.toCharMaybe

unicodeToCharUnsafe :: Unicode.Char -> Char
unicodeToCharUnsafe = ASCII.Superset.toCharUnsafe

{- $unicodeStringConversions

These functions convert between @['Char']@ (a list of ASCII characters) and 'Unicode.String' (a list of Unicode characters).

-}

charListToUnicodeString :: [Char] -> Unicode.String
charListToUnicodeString = ASCII.Superset.fromCharList

unicodeStringToCharListMaybe :: Unicode.String -> Maybe [Char]
unicodeStringToCharListMaybe = ASCII.Superset.toCharListMaybe

unicodeStringToCharListUnsafe :: Unicode.String -> [Char]
unicodeStringToCharListUnsafe = ASCII.Superset.toCharListUnsafe

{- $textConversions

These functions convert between @['Char']@ and 'Text.Text'.

-}

{- |

>>> charListToText [CapitalLetterH,SmallLetterI,ExclamationMark]
"Hi!"

-}

charListToText :: [Char] -> Text.Text
charListToText = ASCII.Superset.fromCharList

textToCharListMaybe :: Text.Text -> Maybe [Char]
textToCharListMaybe = ASCII.Superset.toCharListMaybe

textToCharListUnsafe :: Text.Text -> [Char]
textToCharListUnsafe = ASCII.Superset.toCharListUnsafe

{- $byteStringConversions

These functions convert between @['Char']@ and 'BS.ByteString'.

-}

charListToByteString :: [Char] -> BS.ByteString
charListToByteString = ASCII.Superset.fromCharList

byteStringToCharListMaybe :: BS.ByteString -> Maybe [Char]
byteStringToCharListMaybe = ASCII.Superset.toCharListMaybe

byteStringToCharListUnsafe :: BS.ByteString -> [Char]
byteStringToCharListUnsafe = ASCII.Superset.toCharListUnsafe

{- $refinement

See also: "ASCII.Refinement"

-}

{- $lift

See also: "ASCII.Lift"

-}

{- | Converts from ASCII to any larger type.

For example, @(lift \@ASCII.Char \@Word8)@ is the same function as 'charToWord8'.

>>> lift CapitalLetterA :: Word8
65

And @(lift \@[ASCII.Char] \@Text)@ is equivalent to 'charListToText'.

>>> lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text
"Hi!"

Due to the highly polymorphic nature of the 'lift' function, often it must used with an explicit type signature or type application to avoid any type ambiguity.

-}

lift :: Lift ascii superset => ascii -> superset
lift = ASCII.Lift.lift

{- $supersetConversions

These functions all convert from one ASCII-superset type to another, failing if any of the characters in the input is outside the ASCII character set.

-}

convertCharMaybe :: (CharSuperset char1, CharSuperset char2) => char1 -> Maybe char2
convertCharMaybe = ASCII.Superset.convertCharMaybe

convertCharOrFail :: (CharSuperset char1, CharSuperset char2, MonadFail context) => char1 -> context char2
convertCharOrFail = ASCII.Superset.convertCharOrFail

convertStringMaybe :: (StringSuperset char1, StringSuperset char2) => char1 -> Maybe char2
convertStringMaybe = ASCII.Superset.convertStringMaybe

convertStringOrFail :: (StringSuperset char1, StringSuperset char2, MonadFail context) => char1 -> context char2
convertStringOrFail = ASCII.Superset.convertStringOrFail

{- $monoSupersetConversions

These functions are all specializations of 'convertStringMaybe'. They convert a string from one ASCII-superset type to another.

>>> ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x50]
Just "HTTP"

If any of the characters in the input is outside the ASCII character set, the result is 'Nothing'.

>>> ASCII.byteListToUnicodeStringMaybe [0x48, 0x54, 0x54, 0x80]
Nothing

-}

byteStringToUnicodeStringMaybe :: BS.ByteString -> Maybe Unicode.String
byteStringToUnicodeStringMaybe = convertStringMaybe

unicodeStringToByteStringMaybe :: Unicode.String -> Maybe BS.ByteString
unicodeStringToByteStringMaybe = convertStringMaybe

byteListToUnicodeStringMaybe :: [Word8] -> Maybe Unicode.String
byteListToUnicodeStringMaybe = convertStringMaybe

unicodeStringToByteListMaybe :: Unicode.String -> Maybe [Word8]
unicodeStringToByteListMaybe = convertStringMaybe
