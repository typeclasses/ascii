{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ascii.Blaze where

import Data.Ascii.ByteString

-- base
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)

-- blaze-builder
import qualified Blaze.ByteString.Builder as Blaze

newtype AsciiBuilder = AsciiBuilder (Blaze.Builder)
    deriving (Semigroup, Monoid)

unsafeFromBuilder :: Blaze.Builder -> AsciiBuilder
unsafeFromBuilder = AsciiBuilder

toBuilder :: AsciiBuilder -> Blaze.Builder
toBuilder (AsciiBuilder b) = b

toAsciiBuilder :: Ascii -> AsciiBuilder
toAsciiBuilder (Ascii bs) = AsciiBuilder $ Blaze.fromByteString bs

fromAsciiBuilder :: AsciiBuilder -> Ascii
fromAsciiBuilder (AsciiBuilder b) = Ascii $ Blaze.toByteString b
