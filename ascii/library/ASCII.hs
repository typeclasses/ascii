{-| The __American Standard Code for Information Interchange__ (ASCII) comprises
a set of 128 characters, each represented by 7 bits. 33 of these characters are
/'Control' codes/; a few of these are still in use, but most are obsolete relics
of the early days of computing. The other 95 are /'Printable' characters/ such
as letters and numbers, mostly corresponding to the keys on an American English
keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as
UTF-8 that can represent the entire Unicode character set, which includes over a
hundred thousand characters and is not limited to the symbols of any particular
writing system or culture. However, ASCII is still relevant to network
protocols; for example, we can see it in the specification of
<https://www.rfc-editor.org/rfc/rfc9110.html#name-syntax-notation HTTP>.

There is a convenient relationship between ASCII and Unicode: the ASCII
characters are the first 128 characters of the much larger Unicode character set.
The <https://www.unicode.org/charts/PDF/U0000.pdf C0 Controls and Basic Latin>
section of the Unicode standard contains a list of all the ASCII characters.
You may also find this list replicated in the "ASCII.Char" module; each ASCII
character corresponds to a constructor of the 'ASCII.Char' type.

We do not elaborate on the semantics of the control characters here,
because this information is both obsolete and restricted by copyright law.
It is described by a document entitled
/"Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)"/,
published by American National Standards Institute (ANSI) and available for purchase
<https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012 on their website>.
-}
module ASCII
  (
    {- * @Char@ -}
    {- ** ASCII -} {- $char -} Char,
    {- ** Unicode -} UnicodeChar,
    {- ** Case-insensitive -} {- $caselessChar -} CaselessChar,

    {- * Character classifications -}
    {- ** Print/control groups -} {- $groups -}
    Group (..), charGroup,  inGroup,
    {- ** Upper/lower case -} {- $case -}
    Case (..), letterCase, isCase, toCaseChar, toCaseString,
    {- ** Letters  -} isLetter,
    {- ** Letters and numbers  -} isAlphaNum,
    {- ** Decimal digits -} {- $digit -} isDigit, Digit,
    {- ** Hexadecimal digits -} {- $hexchar -} isHexDigit, HexChar,
    {- ** Octal digits -} isOctDigit,
    {- ** Spaces and symbols   -} isSpace, isPunctuation, isSymbol, isVisible,

    {- * Monomorphic character conversions -} {- $monomorphicConversions -}
    {- ** @ASCII.Char@ ↔ @Int@ -} {- $intConversions -}
    charToInt, intToCharMaybe, intToCharUnsafe,
    {- ** @ASCII.Char@ ↔ @Word8@ -} {- $word8Conversions -}
    charToWord8, word8ToCharMaybe, word8ToCharUnsafe,
    {- ** @ASCII.Char@ ↔ @UnicodeChar@ -} {- $unicodeCharConversions -}
    charToUnicode, unicodeToCharMaybe, unicodeToCharUnsafe,

    {- * Monomorphic digit conversions -}
    {- ** @Digit@ ↔ @Word8@ -} {- $digitWord8Conversions -}
    digitToWord8, word8ToDigitMaybe, word8ToDigitUnsafe,
    {- ** @Digit@ ↔ @ASCII.Char@ -} {- $digitCharConversions -}
    digitToChar, charToDigitMaybe, charToDigitUnsafe,
    {- ** @Digit@ ↔ @UnicodeChar@ -} {- $digitUnicodeConversions -}
    digitToUnicode, unicodeToDigitMaybe, unicodeToDigitUnsafe,
    {- ** @HexChar@ ↔ @Word8@ -} {- $hexCharWord8Conversions -}
    hexCharToWord8, word8ToHexCharMaybe, word8ToHexCharUnsafe,
    {- ** @HexChar@ ↔ @ASCII.Char@ -} {- $hexCharCharConversions -}
    hexCharToChar, charToHexCharMaybe, charToHexCharUnsafe,
    {- ** @HexChar@ ↔ @UnicodeChar@ -} {- $hexCharUnicodeConversions -}
    hexCharToUnicode, unicodeToHexCharMaybe, unicodeToHexCharUnsafe,

    {- * Monomorphic string conversions -}
    {- ** @ASCII.Char@ ↔ @String@ -} {- $unicodeStringConversions -}
    charListToUnicodeString, unicodeStringToCharListMaybe, unicodeStringToCharListUnsafe,
    {- ** @ASCII.Char@ ↔ @Text@ -} {- $textConversions -}
    charListToText, textToCharListMaybe, textToCharListUnsafe,
    {- ** @ASCII.Char@ ↔ @ByteString@ -} {- $byteStringConversions -}
    charListToByteString, byteStringToCharListMaybe, byteStringToCharListUnsafe,

    {- * Monomorphic conversions between ASCII supersets -} {- $monoSupersetConversions -}
    {- ** @ByteString@ ↔ @String@ -}
    byteStringToUnicodeStringMaybe, unicodeStringToByteStringMaybe,
    {- ** @[Word8]@ ↔ @String@ -}
    byteListToUnicodeStringMaybe, unicodeStringToByteListMaybe,

    {- * Monomorphic numeric string conversions -}
    {- ** @Natural@ ↔ @[Digit]@ -}
    showNaturalDigits, readNaturalDigits,
    {- ** @Natural@ ↔ @[HexChar]@ -}
    showNaturalHexChars, readNaturalHexChars,

    {- * Refinement type -} {- $refinement -} ASCII,

    {- * Polymorphic conversions -}
    {- ** Narrowing -} toAsciiCharMaybe, toDigitMaybe, toHexCharMaybe,
    {- ** Validate -} validateChar, validateString,
    {- ** Lift -} {- $lift -} lift,
    {- ** Convert -} {- $supersetConversions -}
    convertCharMaybe, convertCharOrFail, convertStringMaybe, convertStringOrFail,
    {- ** Integral strings -} {- $numbers -}
    showIntegralDecimal, showIntegralHexadecimal,
    readIntegralDecimal, readIntegralHexadecimal,
    {- ** Natural strings -}
    showNaturalDecimal, showNaturalHexadecimal,
    readNaturalDecimal, readNaturalHexadecimal,
    {- ** Single-digit strings -}
    digitString, hexCharString,

    {- * Classes -}
    {- ** Supersets of ASCII -} CharSuperset, StringSuperset, Lift,
    {- ** Equivalents to ASCII -} CharIso, StringIso,
    {- ** Supersets of numeric characters -}
    DigitSuperset, DigitStringSuperset, HexCharSuperset, HexStringSuperset,

    {- * Quasi-quoters -} char, string, caseless, lower, upper,
  )
  where

import ASCII.Case (Case (..))
import ASCII.Caseless (CaselessChar)
import ASCII.Char (Char)
import ASCII.Decimal (Digit, DigitStringSuperset, DigitSuperset)
import ASCII.Group (Group (..))
import ASCII.Hexadecimal (HexChar, HexCharSuperset, HexStringSuperset)
import ASCII.Isomorphism (CharIso, StringIso)
import ASCII.Lift (Lift)
import ASCII.QuasiQuoters (char, string, caseless, lower, upper)
import ASCII.Refinement (ASCII, validateChar, validateString)
import ASCII.Superset (CharSuperset, StringSuperset)

import Control.Monad ((>=>))
import Control.Monad.Fail (MonadFail)
import Data.Bits (Bits)
import Data.Bool (Bool (..))
import Data.Foldable (any)
import Data.Function ((.))
import Data.Int (Int)
import Data.Maybe (Maybe, maybe)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Prelude (Integral)

import qualified ASCII.Case
import qualified ASCII.Decimal
import qualified ASCII.Group
import qualified ASCII.Hexadecimal
import qualified ASCII.Isomorphism
import qualified ASCII.Lift
import qualified ASCII.Predicates
import qualified ASCII.Superset

import qualified Data.ByteString as BS
import qualified Data.Char as Unicode
import qualified Data.String as Unicode
import qualified Data.Text as Text

{- $char

See also: "ASCII.Char"

-}

{- $caselessChar

See also: "ASCII.Caseless"

-}

-- | A character in the full range of Unicode
--
-- ASCII 'Char' is a subset of this type. Convert using 'charToUnicode' and 'unicodeToCharMaybe'.
type UnicodeChar = Unicode.Char

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

These functions convert between the ASCII 'Char' type and the 'UnicodeChar' type.

-}

charToUnicode :: Char -> UnicodeChar
charToUnicode = ASCII.Superset.fromChar

unicodeToCharMaybe :: UnicodeChar -> Maybe Char
unicodeToCharMaybe = ASCII.Superset.toCharMaybe

unicodeToCharUnsafe :: UnicodeChar -> Char
unicodeToCharUnsafe = ASCII.Superset.toCharUnsafe

{- $digitWord8Conversions

These functions convert between the ASCII 'Digit' type and ASCII digits in their byte encoding.

These conversions do /not/ correspond to the numeric interpretations of 'Digit' and 'Word8'. For example, 'digitToWord8' 'ASCII.Decimal.Digit0' is 48, not 0.

-}

digitToWord8 :: Digit -> Word8
digitToWord8 = ASCII.Decimal.fromDigit

word8ToDigitMaybe :: Word8 -> Maybe Digit
word8ToDigitMaybe = ASCII.Decimal.toDigitMaybe

word8ToDigitUnsafe :: Word8 -> Digit
word8ToDigitUnsafe = ASCII.Decimal.toDigitUnsafe

{- $digitCharConversions

These functions convert between the ASCII 'Digit' type and the ASCII 'Char' type.

-}

digitToChar :: Digit -> Char
digitToChar = ASCII.Decimal.fromDigit

charToDigitMaybe :: Char -> Maybe Digit
charToDigitMaybe = ASCII.Decimal.toDigitMaybe

charToDigitUnsafe :: Char -> Digit
charToDigitUnsafe = ASCII.Decimal.toDigitUnsafe

{- $digitUnicodeConversions

These functions convert between the ASCII 'Digit' type and the 'UnicodeChar' type.

-}

digitToUnicode :: Digit -> UnicodeChar
digitToUnicode = ASCII.Decimal.fromDigit

unicodeToDigitMaybe :: UnicodeChar -> Maybe Digit
unicodeToDigitMaybe = ASCII.Decimal.toDigitMaybe

unicodeToDigitUnsafe :: UnicodeChar -> Digit
unicodeToDigitUnsafe = ASCII.Decimal.toDigitUnsafe


{- $hexCharWord8Conversions

These functions convert between the ASCII 'HexChar' type and ASCII characters in their byte encoding.

These conversions do /not/ correspond to the numeric interpretations of 'HexChar' and 'Word8'. For example, 'hexCharToWord8' 'ASCII.Hexadecimal.CapitalLetterA' is 65, not 10.

-}

hexCharToWord8 :: HexChar -> Word8
hexCharToWord8 = ASCII.Hexadecimal.fromHexChar

word8ToHexCharMaybe :: Word8 -> Maybe HexChar
word8ToHexCharMaybe = ASCII.Hexadecimal.toHexCharMaybe

word8ToHexCharUnsafe :: Word8 -> HexChar
word8ToHexCharUnsafe = ASCII.Hexadecimal.toHexCharUnsafe

{- $hexCharCharConversions

These functions convert between the ASCII 'HexChar' type and the ASCII 'Char' type.

-}

hexCharToChar :: HexChar -> Char
hexCharToChar = ASCII.Hexadecimal.fromHexChar

charToHexCharMaybe :: Char -> Maybe HexChar
charToHexCharMaybe = ASCII.Hexadecimal.toHexCharMaybe

charToHexCharUnsafe :: Char -> HexChar
charToHexCharUnsafe = ASCII.Hexadecimal.toHexCharUnsafe

{- $hexCharUnicodeConversions

These functions convert between the ASCII 'HexChar' type and the 'UnicodeChar' type.

-}

hexCharToUnicode :: HexChar -> UnicodeChar
hexCharToUnicode = ASCII.Hexadecimal.fromHexChar

unicodeToHexCharMaybe :: UnicodeChar -> Maybe HexChar
unicodeToHexCharMaybe = ASCII.Hexadecimal.toHexCharMaybe

unicodeToHexCharUnsafe :: UnicodeChar -> HexChar
unicodeToHexCharUnsafe = ASCII.Hexadecimal.toHexCharUnsafe

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

convertStringMaybe :: (StringSuperset string1, StringSuperset string2) => string1 -> Maybe string2
convertStringMaybe = ASCII.Superset.convertStringMaybe

convertStringOrFail :: (StringSuperset string1, StringSuperset string2, MonadFail context) => string1 -> context string2
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

{- | Returns True for ASCII letters:

- 'ASCII.Char.SmallLetterA' to 'ASCII.Char.SmallLetterZ'
- 'ASCII.Char.CapitalLetterA' to 'ASCII.Char.CapitalLetterZ'

-}

isLetter :: CharSuperset char => char -> Bool
isLetter x = any ASCII.Predicates.isLetter (convertCharMaybe x)

{- | Returns True for the characters from 'ASCII.Char.Digit0' to 'ASCII.Char.Digit9'. -}

isDigit :: CharSuperset char => char -> Bool
isDigit x = any ASCII.Predicates.isDigit (convertCharMaybe x)


{- | Returns True for the characters from 'ASCII.Char.Digit0' to 'ASCII.Char.Digit7'. -}

isOctDigit :: CharSuperset char => char -> Bool
isOctDigit x = any ASCII.Predicates.isOctDigit (convertCharMaybe x)


{- | Returns True for characters in any of the following ranges:

- 'ASCII.Char.Digit0' to 'ASCII.Char.Digit9'
- 'ASCII.Char.CapitalLetterA' to 'ASCII.Char.CapitalLetterF'
- 'ASCII.Char.SmallLetterA' to 'ASCII.Char.SmallLetterF'

-}

isHexDigit :: CharSuperset char => char -> Bool
isHexDigit x = any ASCII.Predicates.isHexDigit (convertCharMaybe x)

{- | Returns True for the following characters:

- 'ASCII.Char.Space'
- 'ASCII.Char.HorizontalTab'
- 'ASCII.Char.LineFeed'
- 'ASCII.Char.VerticalTab'
- 'ASCII.Char.FormFeed'
- 'ASCII.Char.CarriageReturn'

-}

isSpace :: CharSuperset char => char -> Bool
isSpace x = any ASCII.Predicates.isSpace (convertCharMaybe x)

{- | Returns True if the character is either an ASCII letter ('isLetter') or an ASCII digit ('isDigit'). -}

isAlphaNum :: CharSuperset char => char -> Bool
isAlphaNum x = any ASCII.Predicates.isAlphaNum (convertCharMaybe x)

{- | Returns True for the following characters:

- 'ASCII.Char.ExclamationMark'
- 'ASCII.Char.QuotationMark'
- 'ASCII.Char.NumberSign'
- 'ASCII.Char.PercentSign'
- 'ASCII.Char.Ampersand'
- 'ASCII.Char.Apostrophe'
- 'ASCII.Char.LeftParenthesis'
- 'ASCII.Char.RightParenthesis'
- 'ASCII.Char.Asterisk'
- 'ASCII.Char.Comma'
- 'ASCII.Char.HyphenMinus'
- 'ASCII.Char.FullStop'
- 'ASCII.Char.Slash'
- 'ASCII.Char.Colon'
- 'ASCII.Char.Semicolon'
- 'ASCII.Char.QuestionMark'
- 'ASCII.Char.AtSign'
- 'ASCII.Char.LeftSquareBracket'
- 'ASCII.Char.Backslash'
- 'ASCII.Char.RightSquareBracket'
- 'ASCII.Char.Underscore'
- 'ASCII.Char.LeftCurlyBracket'
- 'ASCII.Char.RightCurlyBracket'

-}

isPunctuation :: CharSuperset char => char -> Bool
isPunctuation x = any ASCII.Predicates.isPunctuation (convertCharMaybe x)

{- | Returns True for the following characters:

- 'ASCII.Char.DollarSign'
- 'ASCII.Char.PlusSign'
- 'ASCII.Char.LessThanSign'
- 'ASCII.Char.EqualsSign'
- 'ASCII.Char.GreaterThanSign'
- 'ASCII.Char.Caret'
- 'ASCII.Char.GraveAccent'
- 'ASCII.Char.VerticalLine'
- 'ASCII.Char.Tilde'

-}

isSymbol :: CharSuperset char => char -> Bool
isSymbol x = any ASCII.Predicates.isSymbol (convertCharMaybe x)

{- | Returns True for visible characters.

This includes all print characters except 'ASCII.Char.Space'.

-}

isVisible :: CharSuperset char => char -> Bool
isVisible x = any ASCII.Predicates.isVisible (convertCharMaybe x)

{- $numbers

See also: "ASCII.Decimal" and "ASCII.Hexadecimal"

-}

{- |

Gives the ASCII string representation of an integer in decimal (base 10)
notation, using digits 'ASCII.Char.Digit0' through 'ASCII.Char.Digit9',
leading with 'ASCII.Char.HyphenMinus' for negative numbers.

For example, @'showIntegralDecimal' (-512 :: 'Prelude.Integer')@ = @"-512"@.

-}

showIntegralDecimal :: (Integral n, StringSuperset string) => n -> string
showIntegralDecimal = ASCII.Decimal.showIntegral

{- |

Gives the ASCII string representation of an integer in hexadecimal (base 16)
notation, using digits 'ASCII.Char.Digit0' through 'ASCII.Char.Digit9', for
digits 0 though 9. The representation of digits 10 to 15 is determined by the
value of 'Case' parameter: 'UpperCase' means 'ASCII.Char.CapitalLetterA' to
'ASCII.Char.CapitalLetterF', and 'LowerCase' means 'ASCII.Char.SmallLetterA' to
'ASCII.Char.SmallLetterF'. For negative numbers, the resulting string begins
with 'ASCII.Char.HyphenMinus'.

For example, @'showIntegralHexadecimal' 'UpperCase' ('Prelude.negate' (256 + 12) :: 'Prelude.Integer')@ = @"-10C"@.

-}

showIntegralHexadecimal :: (Integral n, StringSuperset string) => Case -> n -> string
showIntegralHexadecimal = ASCII.Hexadecimal.showIntegral

{- |

Roughly the inverse of 'showIntegralDecimal'

* Leading zeroes are accepted, as in @"0074"@ and @"-0074"@

Conditions where the result is 'Data.Maybe.Nothing':

* If the input is empty
* If the input contains any other extraneous characters
* If the resulting number would be outside the range supported by the 'Integral' (determined by its 'Bits' instance)

-}

readIntegralDecimal :: (StringSuperset string, Integral number, Bits number) => string -> Maybe number
readIntegralDecimal = ASCII.Decimal.readIntegral

{- |

Roughly the inverse of 'showIntegralHexadecimal'

* Upper and lower case letters are treated equally
* Leading zeroes are accepted, as in @"006a"@ and @"-006a"@

Conditions where the result is 'Data.Maybe.Nothing':

* If the input is empty
* If the input contains any other extraneous characters
* If the resulting number would be outside the range supported by the 'Integral' (determined by its 'Bits' instance)

-}

readIntegralHexadecimal :: (StringSuperset string, Integral number, Bits number) => string -> Maybe number
readIntegralHexadecimal = ASCII.Hexadecimal.readIntegral

{- |

Gives the ASCII string representation of an natural number in decimal (base 10)
notation, using digits 'ASCII.Char.Digit0' through 'ASCII.Char.Digit9'.

For example, @'showNaturalDecimal' 512@ = @"512"@.

-}

showNaturalDecimal :: DigitStringSuperset string => Natural -> string
showNaturalDecimal = ASCII.Decimal.showNatural

{- |

Gives the ASCII string representation of an integer in hexadecimal (base 16)
notation, using digits 'ASCII.Char.Digit0' through 'ASCII.Char.Digit9', for
digits 0 though 9. The representation of digits 10 to 15 is determined by the
value of 'Case' parameter: 'UpperCase' means 'ASCII.Char.CapitalLetterA' to
'ASCII.Char.CapitalLetterF', and 'LowerCase' means 'ASCII.Char.SmallLetterA' to
'ASCII.Char.SmallLetterF'.

For example, @'showNaturalHexadecimal' 'UpperCase' (256 + 12)@ = @"10C"@.

-}

showNaturalHexadecimal :: HexStringSuperset string => Case -> Natural -> string
showNaturalHexadecimal = ASCII.Hexadecimal.showNatural

{- |

Roughly the inverse of 'showNaturalDecimal'

* Leading zeroes are accepted, as in @"0074"@

Conditions where the result is 'Data.Maybe.Nothing':

* If the input is empty
* If the input contains any other extraneous characters

-}

readNaturalDecimal :: DigitStringSuperset string => string -> Maybe Natural
readNaturalDecimal = ASCII.Decimal.readNatural

{- |

Roughly the inverse of 'showNaturalHexadecimal'

* Upper and lower case letters are treated equally
* Leading zeroes are accepted, as in @"006a"@

Conditions where the result is 'Data.Maybe.Nothing':

* If the input is empty
* If the input contains any other extraneous characters

-}

readNaturalHexadecimal :: HexStringSuperset string => string -> Maybe Natural
readNaturalHexadecimal = ASCII.Hexadecimal.readNatural

{- $digit

See also: "ASCII.Decimal"

-}

{- $hexchar

See also: "ASCII.Hexadecimal"

-}

{- |

Specialization of 'showNaturalDecimal'

See also: 'showIntegralDecimal'

-}

showNaturalDigits :: Natural -> [Digit]
showNaturalDigits = showNaturalDecimal

{- |

Specialization of 'readNaturalDecimal'

See also: 'readIntegralDecimal'

-}

readNaturalDigits :: [Digit] -> Maybe Natural
readNaturalDigits = readNaturalDecimal

{- |

Specialization of 'showNaturalHexadecimal'

See also: 'showIntegralHexadecimal'

-}

showNaturalHexChars :: Case -> Natural -> [HexChar]
showNaturalHexChars = showNaturalHexadecimal

{- |

Specialization of 'readNaturalHexadecimal'

See also: 'readIntegralHexadecimal'

-}

readNaturalHexChars :: [HexChar] -> Maybe Natural
readNaturalHexChars = readNaturalHexadecimal

{- |

A string containing a single digit character 0-9

-}
digitString :: DigitStringSuperset string => Digit -> string
digitString x = ASCII.Decimal.fromDigitList [x]

{- |

A string containing a single hexadecimal digit character 0-9, A-F, or a-f.

-}
hexCharString :: HexStringSuperset string => HexChar -> string
hexCharString x = ASCII.Hexadecimal.fromHexCharList [x]

toAsciiCharMaybe :: CharSuperset char => char -> Maybe Char
toAsciiCharMaybe = ASCII.Superset.toCharMaybe

toDigitMaybe :: DigitSuperset char => char -> Maybe Digit
toDigitMaybe = ASCII.Decimal.toDigitMaybe

toHexCharMaybe :: HexCharSuperset char => char -> Maybe HexChar
toHexCharMaybe = ASCII.Hexadecimal.toHexCharMaybe
