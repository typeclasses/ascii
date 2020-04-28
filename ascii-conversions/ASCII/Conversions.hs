module ASCII.Conversions ( AsciiCharSuperset (..) ) where

import ASCII.Char (Char)
import qualified ASCII.Char

import Prelude (Maybe, (.))
import qualified Prelude

import qualified Data.Char as Unicode
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word

-- | Conversions between a 'Char' and some other larger type such as the standard Unicode 'Unicode.Char' type in "Data.Char".
--
-- Instances should follow these rules:
--
-- - @toCharMaybe (fromChar c)@ = @Just c@
-- - If @toCharMaybe x@ = @Just c@, then @toCharSub x@ = @c@
-- - If @toCharMaybe x@ = @Just c@, then @toCharUnsafe x@ = @c@
-- - If @toCharMaybe x@ = @Nothing@, then @toCharSub x@ = 'Substitute'

class AsciiCharSuperset a
  where
    {-# MINIMAL fromChar, toCharMaybe #-}

    -- | Converts from 'Char' to some other, larger type such as the standard Unicode 'Unicode.Char' type in "Data.Char".
    fromChar :: Char -> a

    -- | Returns 'Nothing' for any value that does not represent an ASCII character.
    toCharMaybe :: a -> Prelude.Maybe Char

    -- | Returns the 'Substitute' character for any value that does not represent an ASCII character.
    toCharSub :: a -> Char
    toCharSub = Maybe.fromMaybe ASCII.Char.Substitute . toCharMaybe

    -- | Undefined for any value that does not represent an ASCII character.
    toCharUnsafe :: a -> Char
    toCharUnsafe = Maybe.fromJust . toCharMaybe

-- | Representation of an ASCII 'Char' as an 'Int' between 0 and 127.
instance AsciiCharSuperset Prelude.Int
  where
    fromChar = ASCII.Char.toInt
    toCharMaybe = ASCII.Char.fromIntMaybe

-- | Representation of an ASCII 'Char' as a byte where the first bit is always 0.
instance AsciiCharSuperset Word.Word8
  where
    fromChar = Prelude.fromIntegral . ASCII.Char.toInt
    toCharMaybe = ASCII.Char.fromIntMaybe . Prelude.fromIntegral

-- | Representation of an ASCII 'Char' as one of the first 128 Unicode 'Unicode.Char's.
instance AsciiCharSuperset Unicode.Char
  where
    fromChar = Unicode.chr . ASCII.Char.toInt
    toCharMaybe = ASCII.Char.fromIntMaybe . Unicode.ord
