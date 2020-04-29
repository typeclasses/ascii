module ASCII.Superset where

import Data.Bool (Bool, (&&))
import Data.Function ((.))
import Data.Ord ((<=), (>=))

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

class AsciiCharSuperset a
  where
    isAsciiChar :: a -> Bool
    fromChar :: ASCII.Char -> a
    toCharUnsafe :: a -> ASCII.Char

class AsciiStringSuperset a
  where
    isAsciiString :: a -> Bool
    fromCharList :: [ASCII.Char] -> a
    toCharListUnsafe :: a -> [ASCII.Char]
    toCharListSub :: a -> [ASCII.Char]

toCharSub :: AsciiCharSuperset a => a -> ASCII.Char
toCharSub x = if isAsciiChar x then toCharUnsafe x else ASCII.Substitute

instance AsciiCharSuperset Unicode.Char
  where
    isAsciiChar = (<= '\DEL')
    fromChar = Unicode.chr . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Unicode.ord

instance AsciiCharSuperset Nat.Natural
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance AsciiCharSuperset Int.Int
  where
    isAsciiChar x = (x >= 0) && (x <= 127)
    fromChar = ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe

instance AsciiCharSuperset Word.Word8
  where
    isAsciiChar = (<= 127)
    fromChar = Prelude.fromIntegral . ASCII.toInt
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance AsciiCharSuperset a => AsciiStringSuperset [a]
  where
    isAsciiString = List.all isAsciiChar
    fromCharList = List.map fromChar
    toCharListUnsafe = List.map toCharUnsafe
    toCharListSub = List.map toCharSub

instance AsciiStringSuperset T.Text
  where
    isAsciiString = T.all isAsciiChar
    fromCharList = T.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . T.unpack
    toCharListSub = toCharListSub . T.unpack

instance AsciiStringSuperset LT.Text
  where
    isAsciiString = LT.all isAsciiChar
    fromCharList = LT.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LT.unpack
    toCharListSub = toCharListSub . LT.unpack

instance AsciiStringSuperset TB.Builder
  where
    isAsciiString = isAsciiString . TB.toLazyText
    fromCharList = TB.fromString . fromCharList
    toCharListUnsafe = toCharListUnsafe . TB.toLazyText
    toCharListSub = toCharListSub . TB.toLazyText

instance AsciiStringSuperset BS.ByteString
  where
    isAsciiString = BS.all isAsciiChar
    fromCharList = BS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . BS.unpack
    toCharListSub = toCharListSub . BS.unpack

instance AsciiStringSuperset LBS.ByteString
  where
    isAsciiString = LBS.all isAsciiChar
    fromCharList = LBS.pack . fromCharList
    toCharListUnsafe = toCharListUnsafe . LBS.unpack
    toCharListSub = toCharListSub . LBS.unpack

instance AsciiStringSuperset BSB.Builder
  where
    isAsciiString = isAsciiString . BSB.toLazyByteString
    fromCharList = BSB.lazyByteString . fromCharList
    toCharListUnsafe = toCharListUnsafe . BSB.toLazyByteString
    toCharListSub = toCharListSub . BSB.toLazyByteString
