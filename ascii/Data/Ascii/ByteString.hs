{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Data.Ascii.ByteString where

-- base
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.String (IsString (..))
import qualified Data.Char as C
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)

-- bytestring
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

-- case-insensitive
import Data.CaseInsensitive (FoldCase, CI, mk, original)

-- hashable
import Data.Hashable (Hashable)

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

newtype Ascii = Ascii ByteString
    deriving (Show, Eq, Read, Ord, Data, Typeable, IsString, FoldCase, Hashable, Semigroup, Monoid)

type CIAscii = CI Ascii

fromByteString :: ByteString -> Maybe Ascii
fromByteString bs
    | S.all (< 128) bs = Just $ Ascii bs
    | otherwise = Nothing

-- | Renamed to avoid clash with 'fromString'
fromChars :: String -> Maybe Ascii
fromChars s
    | all C.isAscii s = Just $ Ascii $ S8.pack s
    | otherwise = Nothing

fromText :: Text -> Maybe Ascii
fromText t
    | T.all C.isAscii t = Just $ Ascii $ TE.encodeUtf8 t
    | otherwise = Nothing

unsafeFromByteString :: ByteString -> Ascii
unsafeFromByteString = Ascii

unsafeFromString :: String -> Ascii
unsafeFromString = Ascii . S8.pack

unsafeFromText :: Text -> Ascii
unsafeFromText = Ascii . TE.encodeUtf8

toCIAscii :: Ascii -> CIAscii
toCIAscii = mk

fromCIAscii :: CIAscii -> Ascii
fromCIAscii = original

toByteString :: Ascii -> ByteString
toByteString (Ascii bs) = bs

toString :: Ascii -> String
toString (Ascii bs) = S8.unpack bs

toText :: Ascii -> Text
toText (Ascii bs) = TE.decodeUtf8 bs

ciToByteString :: CIAscii -> ByteString
ciToByteString = toByteString . original
