module ASCII.Superset where

import Control.Monad (return)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bool (Bool, (&&))
import Data.Function ((.))
import Data.Ord ((<=), (>=))
import Data.Maybe (Maybe (..))

import qualified ASCII.Char as ASCII
import qualified Data.Char as Unicode
import qualified Data.Word as Word
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Numeric.Natural as Nat
import qualified Prelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB


---  Char  ---

class IsChar a
  where
    isAsciiChar :: a -> Bool
    fromChar :: ASCII.Char -> a
    toCharUnsafe :: a -> ASCII.Char

toCharMaybe :: IsChar a => a -> Maybe ASCII.Char
toCharMaybe = toCharOrFail

toCharOrFail :: (IsChar a, MonadFail m) => a -> m ASCII.Char
toCharOrFail x = if isAsciiChar x then return (toCharUnsafe x) else fail "Not an ASCII character"

toCharSub :: IsChar a => a -> ASCII.Char
toCharSub x = if isAsciiChar x then toCharUnsafe x else ASCII.Substitute

substituteChar :: IsChar a => a -> a
substituteChar x = if isAsciiChar x then x else fromChar ASCII.Substitute


---  String  ---

class IsString a
  where
    isAsciiString :: a -> Bool
    fromCharList :: [ASCII.Char] -> a
    toCharListUnsafe :: a -> [ASCII.Char]
    toCharListSub :: a -> [ASCII.Char]
    substituteString :: a -> a

toCharListMaybe :: IsString a => a -> Maybe [ASCII.Char]
toCharListMaybe = toCharListOrFail

toCharListOrFail :: (IsString a, MonadFail m) => a -> m [ASCII.Char]
toCharListOrFail x = if isAsciiString x then return (toCharListUnsafe x) else fail "String contains non-ASCII characters"


---  Instances  ---

instance IsChar Unicode.Char
  where
    isAsciiChar = (<= '\DEL')
    fromChar = Unicode.chr . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Unicode.ord

instance IsChar Nat.Natural
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance IsChar Int.Int
  where
    isAsciiChar x = (x >= 0) && (x <= 127)
    fromChar = ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe

instance IsChar Word.Word8
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance IsChar a => IsString [a]
  where
    isAsciiString = List.all isAsciiChar
    fromCharList = List.map fromChar
    toCharListUnsafe = List.map toCharUnsafe
    toCharListSub = List.map toCharSub
    substituteString = List.map substituteChar

instance IsString T.Text
  where
    isAsciiString = T.all isAsciiChar
    fromCharList = T.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . T.unpack
    toCharListSub = toCharListSub . T.unpack
    substituteString = T.map substituteChar

instance IsString LT.Text
  where
    isAsciiString = LT.all isAsciiChar
    fromCharList = LT.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LT.unpack
    toCharListSub = toCharListSub . LT.unpack
    substituteString = LT.map substituteChar

instance IsString TB.Builder
  where
    isAsciiString = isAsciiString . TB.toLazyText
    fromCharList = TB.fromString . fromCharList
    toCharListUnsafe = toCharListUnsafe . TB.toLazyText
    toCharListSub = toCharListSub . TB.toLazyText
    substituteString = TB.fromLazyText . substituteString . TB.toLazyText

instance IsString BS.ByteString
  where
    isAsciiString = BS.all isAsciiChar
    fromCharList = BS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . BS.unpack
    toCharListSub = toCharListSub . BS.unpack
    substituteString = BS.map substituteChar

instance IsString LBS.ByteString
  where
    isAsciiString = LBS.all isAsciiChar
    fromCharList = LBS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LBS.unpack
    toCharListSub = toCharListSub . LBS.unpack
    substituteString = LBS.map substituteChar

instance IsString BSB.Builder
  where
    isAsciiString = isAsciiString . BSB.toLazyByteString
    fromCharList = BSB.lazyByteString . fromCharList
    toCharListUnsafe = toCharListUnsafe . BSB.toLazyByteString
    toCharListSub = toCharListSub . BSB.toLazyByteString
    substituteString = BSB.lazyByteString . substituteString . BSB.toLazyByteString
