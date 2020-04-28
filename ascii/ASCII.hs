{- |

The __American Standard Code for Information Interchange__ (ASCII) comprises a set of 128 characters, each represented by 7 bits. 33 of these characters are /'Control' codes/; a few of these are still in use, but most are obsolete relics of the early days of computing. The other 95 are /'Printable' characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as UTF-8 that can represent the entire Unicode character set, which includes over a hundred thousand characters and is not limited to the symbols of any particular writing system or culture. However, ASCII is still relevant to network protocols; for example, we can see it in the specification of [HTTP message headers](https://tools.ietf.org/html/rfc7230#section-1.2).

There is a convenient relationship between ASCII and Unicode: the ASCII characters are the first 128 characters of the much larger Unicode character set. The [C0 Controls and Basic Latin](https://www.unicode.org/charts/PDF/U0000.pdf) section of the Unicode standard contains a list of all the ASCII characters. You may also find this list replicated below; each ASCII character corresponds to a constructor of the 'Char' type.

We do not elaborate on the semantics of the control characters here, because this information is both obsolete and restricted by copyright law. It is described by a document entitled /"Coded Character Sets - 7-Bit American National Standard Code for Information Interchange (7-Bit ASCII)"/, published by American National Standards Institute (ANSI) and available for purchase [on their website](https://webstore.ansi.org/Standards/INCITS/INCITS1986R2012).

-}

module ASCII
  ( ASCII.Char (..)

  -- * Character groups
  , ASCII.Group.Group (..), ASCII.Group.charGroup, ASCII.Group.inGroup

  -- * Char/Int conversions
  , charToInt, intToCharMaybe, intToCharUnsafe

  ) where

import qualified ASCII.Char as ASCII (Char)
import qualified ASCII.Char
import qualified ASCII.Group

charToInt :: ASCII.Char -> Int
charToInt = ASCII.Char.toInt

intToCharMaybe :: Int -> Maybe ASCII.Char
intToCharMaybe = ASCII.Char.fromIntMaybe

intToCharUnsafe :: Int -> ASCII.Char
intToCharUnsafe = ASCII.Char.fromIntUnsafe
