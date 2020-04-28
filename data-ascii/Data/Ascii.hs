module Data.Ascii
    ( -- * Datatypes
      Ascii
    , CIAscii
    , AsciiBuilder
      -- * Construction
      -- ** Safe
    , fromByteString
    , fromChars
    , fromText
      -- ** Unsafe
    , unsafeFromByteString
    , unsafeFromString
    , unsafeFromText
      -- * Extraction
    , toByteString
    , toString
    , toText
      -- * Case insensitive
    , toCIAscii
    , fromCIAscii
    , ciToByteString
      -- * Builder
    , toAsciiBuilder
    , fromAsciiBuilder
    , unsafeFromBuilder
    , toBuilder
      -- * Character-level functions and predicates
    , fromChar
    , toChar
    , ascii
    , isAscii
    , isControl
    , isPrintable
    , isWhiteSpace
    , isSpaceOrTab
    , isLower
    , isUpper
    , toLower
    , toUpper
    , isAlpha
    , isDigit
    , isAlphaNum
    , fromDigit
    , unsafeFromDigit
    , fromOctDigit
    , unsafeFromOctDigit
    , isUpHexDigit
    , fromUpHexDigit
    , unsafeFromUpHexDigit
    , isLowHexDigit
    , fromLowHexDigit
    , unsafeFromLowHexDigit
    , isHexDigit
    , fromHexDigit
    , unsafeFromHexDigit
    ) where

import Data.Ascii.Blaze
import Data.Ascii.ByteString
import Data.Ascii.Word8
