{- |

The __American Standard Code for Information Interchange__ (ASCII) comprises a set of 128 characters, each represented by 7 bits. 33 of these characters are /'ASCII.Group.Control' codes/; a few of these are still in use, but most are obsolete relics of the early days of computing. The other 95 are /'ASCII.Group.Printable' characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as UTF-8 that can represent the entire Unicode character set, which includes over a hundred thousand characters and is not limited to the symbols of any particular writing system or culture. However, ASCII is still relevant to network protocols; for example, we can see it in the specification of [HTTP message headers](https://tools.ietf.org/html/rfc7230#section-1.2).

There is a convenient relationship between ASCII and Unicode: the ASCII characters are the first 128 characters of the much larger Unicode character set. The [C0 Controls and Basic Latin](https://www.unicode.org/charts/PDF/U0000.pdf) section of the Unicode standard contains a list of all the ASCII characters. You may also find this list replicated in the "ASCII.Char" module; each ASCII character corresponds to a constructor of the 'ASCII.Char' type.

We do not elaborate on the semantics of the control characters here, because this information is both obsolete and restricted by copyright law. It is described by a document entitled /"Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)"/, published by American National Standards Institute (ANSI) and available for purchase [on their website](https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012).

-}

module ASCII
  (

  -- * The ASCII @Char@ type
    Char

  -- * Character classifications

  -- ** Print/control groups
  -- $groups
  , ASCII.Group.Group (..), ASCII.Group.charGroup, ASCII.Group.inGroup

  -- ** Upper/lower case
  , ASCII.Case.Case (..), ASCII.Case.letterCase, ASCII.Case.isCase

  -- * Monomorphic conversions

  -- ** Int
  -- $intConversions
  , charToInt, intToCharMaybe, intToCharUnsafe

  -- ** Word8
  -- $word8Conversions
  , charToWord8, word8ToCharMaybe, word8ToCharUnsafe

  -- * Polymorphic conversions

  -- ** Lifting
  -- $lifting
  , liftChar, liftString

  -- ** Refinement
  , ASCII.Refinement.ASCII, ASCII.Refinement.lift, ASCII.Refinement.validateChar, ASCII.Refinement.validateString

  ) where

import Prelude ((.), Maybe, Int)
import qualified Prelude

import Data.Word (Word8)

import ASCII.Char (Char)

import qualified ASCII.Char
import qualified ASCII.Case
import qualified ASCII.Group
import qualified ASCII.Refinement
import qualified ASCII.Superset

{- $setup

>>> import Prelude

-}

{- $groups

ASCII characters are broadly categorized into two groups: /control codes/ and /printable characters/.

-}

{- $intConversions

These functions convert between the ASCII 'Char' type and 'Int'.

-}

{- |

>>> import ASCII.Char
>>> map charToInt [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

charToInt :: Char -> Int
charToInt = ASCII.Char.toInt

intToCharMaybe :: Int -> Maybe Char
intToCharMaybe = ASCII.Char.fromIntMaybe

intToCharUnsafe :: Int -> Char
intToCharUnsafe = ASCII.Char.fromIntUnsafe

{- $word8Conversions

These functions convert between the ASCII 'Char' type and 'Word8'.

-}

{- |

>>> import ASCII.Char
>>> map charToWord8 [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

charToWord8 :: Char -> Word8
charToWord8 = Prelude.fromIntegral . ASCII.Char.toInt

word8ToCharMaybe :: Word8 -> Maybe Char
word8ToCharMaybe = intToCharMaybe . Prelude.fromIntegral

word8ToCharUnsafe :: Word8 -> Char
word8ToCharUnsafe = intToCharUnsafe . Prelude.fromIntegral

{- $lifting

These functions convert from ASCII 'Char' type into some other type that represents a superset of ASCII.

-}

{- | Converts from 'Char' to any larger 'Char'-like type.

For example, @'liftChar' \@'Word8' = 'charToWord8'@.

>>> import Data.Word
>>> liftChar ASCII.Char.CapitalLetterA :: Word8
65

-}

liftChar :: ASCII.Superset.IsChar a => Char -> a
liftChar = ASCII.Superset.fromChar

liftString :: ASCII.Superset.IsString a => [Char] -> a
liftString = ASCII.Superset.fromCharList
