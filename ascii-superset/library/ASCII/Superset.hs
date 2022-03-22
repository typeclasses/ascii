module ASCII.Superset (

    {- * Characters -}
    {- ** Class -} CharSuperset (..),
    {- ** Functions -} asCharUnsafe, toCharMaybe, toCharOrFail, toCharSub, substituteChar, convertCharMaybe, convertCharOrFail,

    {- * Strings -}
    {- ** Class -} StringSuperset (..),
    {- ** Functions -} toCharListMaybe, toCharListOrFail, convertStringMaybe, convertStringOrFail

    ) where

import Control.Monad (return)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bool (Bool, (&&))
import Data.Function (id, (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))
import Data.Ord ((<=), (>=))

import qualified ASCII.Char as ASCII
import qualified Data.Bool as Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Unicode
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Word as Word
import qualified Numeric.Natural as Nat
import qualified Prelude


---  Char  ---

class CharSuperset char
  where

    isAsciiChar :: char -> Bool

    fromChar :: ASCII.Char -> char

    toCharUnsafe :: char -> ASCII.Char

asCharUnsafe :: CharSuperset char => (ASCII.Char -> ASCII.Char) -> char -> char
asCharUnsafe f = fromChar . f . toCharUnsafe

toCharMaybe :: CharSuperset char => char -> Maybe ASCII.Char
toCharMaybe = toCharOrFail

toCharOrFail :: (CharSuperset char, MonadFail context) => char -> context ASCII.Char
toCharOrFail x = if isAsciiChar x then return (toCharUnsafe x) else fail "Not an ASCII character"

toCharSub :: CharSuperset char => char -> ASCII.Char
toCharSub x = if isAsciiChar x then toCharUnsafe x else ASCII.Substitute

substituteChar :: CharSuperset char => char -> char
substituteChar x = if isAsciiChar x then x else fromChar ASCII.Substitute

-- | Convert from one ASCII-superset character type to another via the ASCII 'ASCII.Char' type. Fails as 'Nothing' if the input is outside the ASCII character set.
convertCharMaybe :: (CharSuperset char1, CharSuperset char2) => char1 -> Maybe char2
convertCharMaybe = convertCharOrFail

-- | Convert from one ASCII-superset character type to another via the ASCII 'ASCII.Char' type. Fails with 'fail' if the input is outside the ASCII character set.
convertCharOrFail :: (CharSuperset char1, CharSuperset char2, MonadFail context) => char1 -> context char2
convertCharOrFail = fmap fromChar . toCharOrFail


---  String  ---

class StringSuperset string
  where

    isAsciiString :: string -> Bool

    fromCharList :: [ASCII.Char] -> string

    toCharListUnsafe :: string -> [ASCII.Char]

    toCharListSub :: string -> [ASCII.Char]

    substituteString :: string -> string

    mapCharsUnsafe :: (ASCII.Char -> ASCII.Char) -> string -> string
    mapCharsUnsafe f = fromCharList  . List.map f . toCharListUnsafe

toCharListMaybe :: StringSuperset string => string -> Maybe [ASCII.Char]
toCharListMaybe = toCharListOrFail

toCharListOrFail :: (StringSuperset string, MonadFail context) => string -> context [ASCII.Char]
toCharListOrFail x = if isAsciiString x then return (toCharListUnsafe x) else fail "String contains non-ASCII characters"

-- | Convert from one ASCII-superset string type to another by converting each character of the input string to an ASCII 'ASCII.Char', and then converting the ASCII character list to the desired output type. Fails as 'Nothing' if the input contains any character that is outside the ASCII character set.
convertStringMaybe :: (StringSuperset string1, StringSuperset string2) => string1 -> Maybe string2
convertStringMaybe = convertStringOrFail

-- | Convert from one ASCII-superset string type to another by converting each character of the input string to an ASCII 'ASCII.Char', and then converting the ASCII character list to the desired output type. Fails with 'fail' if the input contains any character that is outside the ASCII character set.
convertStringOrFail :: (StringSuperset string1, StringSuperset string2, MonadFail context) => string1 -> context string2
convertStringOrFail = fmap fromCharList . toCharListOrFail


---  Instances  ---

-- | 'ASCII.Char' is trivially a superset of itself. (This instance is uninteresting.)

instance CharSuperset ASCII.Char
  where
    isAsciiChar _ = Bool.True
    fromChar = id
    toCharUnsafe = id

instance CharSuperset Unicode.Char
  where
    isAsciiChar = (<= '\DEL')
    fromChar = Unicode.chr . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Unicode.ord

instance CharSuperset Nat.Natural
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance CharSuperset Int.Int
  where
    isAsciiChar x = (x >= 0) && (x <= 127)
    fromChar = ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe

instance CharSuperset Word.Word8
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance CharSuperset char => StringSuperset [char]
  where
    isAsciiString = List.all isAsciiChar
    fromCharList = List.map fromChar
    toCharListUnsafe = List.map toCharUnsafe
    toCharListSub = List.map toCharSub
    substituteString = List.map substituteChar

instance StringSuperset T.Text
  where
    isAsciiString = T.all isAsciiChar
    fromCharList = T.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . T.unpack
    toCharListSub = toCharListSub . T.unpack
    substituteString = T.map substituteChar
    mapCharsUnsafe f = T.map (asCharUnsafe f)

instance StringSuperset LT.Text
  where
    isAsciiString = LT.all isAsciiChar
    fromCharList = LT.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LT.unpack
    toCharListSub = toCharListSub . LT.unpack
    substituteString = LT.map substituteChar
    mapCharsUnsafe f = LT.map (asCharUnsafe f)

instance StringSuperset TB.Builder
  where
    isAsciiString = isAsciiString . TB.toLazyText
    fromCharList = TB.fromString . fromCharList
    toCharListUnsafe = toCharListUnsafe . TB.toLazyText
    toCharListSub = toCharListSub . TB.toLazyText
    substituteString = TB.fromLazyText . substituteString . TB.toLazyText
    mapCharsUnsafe f = TB.fromLazyText . mapCharsUnsafe f . TB.toLazyText

instance StringSuperset BS.ByteString
  where
    isAsciiString = BS.all isAsciiChar
    fromCharList = BS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . BS.unpack
    toCharListSub = toCharListSub . BS.unpack
    substituteString = BS.map substituteChar
    mapCharsUnsafe f = BS.map (asCharUnsafe f)

instance StringSuperset LBS.ByteString
  where
    isAsciiString = LBS.all isAsciiChar
    fromCharList = LBS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LBS.unpack
    toCharListSub = toCharListSub . LBS.unpack
    substituteString = LBS.map substituteChar
    mapCharsUnsafe f = LBS.map (asCharUnsafe f)

instance StringSuperset BSB.Builder
  where
    isAsciiString = isAsciiString . BSB.toLazyByteString
    fromCharList = BSB.lazyByteString . fromCharList
    toCharListUnsafe = toCharListUnsafe . BSB.toLazyByteString
    toCharListSub = toCharListSub . BSB.toLazyByteString
    substituteString = BSB.lazyByteString . substituteString . BSB.toLazyByteString
    mapCharsUnsafe f = BSB.lazyByteString . mapCharsUnsafe f . BSB.toLazyByteString
