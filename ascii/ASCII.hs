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

  -- * Refinement type
  , ASCII.Refinement.ASCII

  -- * Polymorphic conversions
  -- ** Validate
  , ASCII.Refinement.validateChar
  , ASCII.Refinement.validateString
  -- ** Lift
  , lift

  -- * Classes
  , IsChar, IsString, ASCII.Lift.Lift

  ) where

import Prelude ((.), Maybe, Int)
import qualified Prelude

import Data.Word (Word8)

import ASCII.Char (Char)
import ASCII.Superset (IsChar, IsString)

import qualified ASCII.Char
import qualified ASCII.Case
import qualified ASCII.Group
import qualified ASCII.Lift
import qualified ASCII.Refinement

{- $setup

>>> import ASCII.Char (Char (..))
>>> import Data.List (map)
>>> import Data.Text (Text)
>>> import Data.Word (Word8)

-}

{- $groups

ASCII characters are broadly categorized into two groups: /control codes/ and /printable characters/.

-}

{- $intConversions

These functions convert between the ASCII 'Char' type and 'Int'.

-}

{- |

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

>>> map charToWord8 [Null, CapitalLetterA, SmallLetterA, Delete]
[0,65,97,127]

-}

charToWord8 :: Char -> Word8
charToWord8 = Prelude.fromIntegral . ASCII.Char.toInt

word8ToCharMaybe :: Word8 -> Maybe Char
word8ToCharMaybe = intToCharMaybe . Prelude.fromIntegral

word8ToCharUnsafe :: Word8 -> Char
word8ToCharUnsafe = intToCharUnsafe . Prelude.fromIntegral

{- | Converts from ASCII to any larger type.

For example, @(lift \@ASCII.Char \@Word8)@ is the same function as 'charToWord8'.

>>> lift CapitalLetterA :: Word8
65

>>> lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text
"Hi!"

Due to the highly polymorphic nature of the 'lift' function, often it must used with an explicit type signature or type application to avoid any type ambiguity.

-}

lift :: ASCII.Lift.Lift ascii superset => ascii -> superset
lift = ASCII.Lift.lift
