{-# LANGUAGE CPP #-}

module Data.Ascii.Word8 where

-- base
import Data.Word (Word8)
import qualified Data.Char as C

fromChar :: Char -> Maybe Word8
fromChar c = if i < 128 then Just (fromIntegral i) else Nothing
  where i = C.ord c

toChar :: Word8 -> Char
toChar = C.chr . fromIntegral

-- | Unsafe version of 'fromChar'
ascii :: Char -> Word8
ascii = fromIntegral . C.ord
{-# INLINE ascii #-}

isAscii :: Word8 -> Bool
isAscii = (< 128)

isControl :: Word8 -> Bool
isControl w = w < 32 || w == 127

isPrintable :: Word8 -> Bool
isPrintable w = w >= 32 && w < 127

isWhiteSpace :: Word8 -> Bool
isWhiteSpace w = w == ascii ' ' || w >= 9 && w <= 13

isSpaceOrTab :: Word8 -> Bool
isSpaceOrTab w = w == ascii ' ' || w == ascii '\t'

isLower :: Word8 -> Bool
isLower w = w >= ascii 'a' && w <= ascii 'z'

isUpper :: Word8 -> Bool
isUpper w = w >= ascii 'A' && w <= ascii 'Z'

toLower :: Word8 -> Word8
toLower w | isUpper w = w + 32
          | otherwise = w

toUpper :: Word8 -> Word8
toUpper w | isLower w = w - 32
          | otherwise = w

isAlpha :: Word8 -> Bool
isAlpha w = isUpper w || isLower w

isDigit :: Word8 -> Bool
isDigit w = w >= ascii '0' && w <= ascii '9'

isAlphaNum :: Word8 -> Bool
isAlphaNum w = isDigit w || isAlpha w

fromDigit :: Num a => Word8 -> Maybe a
fromDigit w | isDigit w = Just $ unsafeFromDigit w
            | otherwise = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromDigit #-}
#endif

unsafeFromDigit :: Num a => Word8 -> a
unsafeFromDigit w = fromIntegral (w - ascii '0')
{-# INLINE unsafeFromDigit #-}

isOctDigit :: Word8 -> Bool
isOctDigit w = w >= ascii '0' && w <= ascii '7'

fromOctDigit :: Num a => Word8 -> Maybe a
fromOctDigit w | isOctDigit w = Just $ unsafeFromOctDigit w
               | otherwise    = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromOctDigit #-}
#endif

unsafeFromOctDigit :: Num a => Word8 -> a
unsafeFromOctDigit = unsafeFromDigit
{-# INLINE unsafeFromOctDigit #-}

isLowAF :: Word8 -> Bool
isLowAF w = w >= ascii 'a' && w <= ascii 'f'
{-# INLINE isLowAF #-}

fromLowAF :: Num a => Word8 -> a
fromLowAF w = fromIntegral (w - ascii 'a' + 10)
{-# INLINE fromLowAF #-}

isLowHexDigit :: Word8 -> Bool
isLowHexDigit w = isDigit w || isLowAF w

fromLowHexDigit :: Num a => Word8 -> Maybe a
fromLowHexDigit w | isDigit w = Just $ unsafeFromDigit w
                  | isLowAF w = Just $ fromLowAF w
                  | otherwise = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromLowHexDigit #-}
#endif

unsafeFromLowHexDigit :: Num a => Word8 -> a
unsafeFromLowHexDigit w | w < ascii 'a' = unsafeFromDigit w
                        | otherwise     = fromLowAF w
{-# INLINE unsafeFromLowHexDigit #-}

isUpAF :: Word8 -> Bool
isUpAF w = w >= ascii 'A' && w <= ascii 'F'
{-# INLINE isUpAF #-}

fromUpAF :: Num a => Word8 -> a
fromUpAF w = fromIntegral (w - ascii 'A' + 10)
{-# INLINE fromUpAF #-}

isUpHexDigit :: Word8 -> Bool
isUpHexDigit w = isDigit w || isUpAF w

fromUpHexDigit :: Num a => Word8 -> Maybe a
fromUpHexDigit w | isDigit w = Just $ unsafeFromDigit w
                 | isUpAF w  = Just $ fromUpAF w
                 | otherwise = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromUpHexDigit #-}
#endif

unsafeFromUpHexDigit :: Num a => Word8 -> a
unsafeFromUpHexDigit w | w < ascii 'A' = unsafeFromDigit w
                       | otherwise     = fromUpAF w
{-# INLINE unsafeFromUpHexDigit #-}

isHexDigit :: Word8 -> Bool
isHexDigit w = isDigit w || isUpAF w || isLowAF w

fromHexDigit :: Num a => Word8 -> Maybe a
fromHexDigit w | isDigit w = Just $ unsafeFromDigit w
               | isUpAF w  = Just $ fromUpAF w
               | isLowAF w = Just $ fromLowAF w
               | otherwise = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromHexDigit #-}
#endif

unsafeFromHexDigit :: Num a => Word8 -> a
unsafeFromHexDigit w | w < ascii 'A' = unsafeFromDigit w
                     | w < ascii 'a' = fromUpAF w
                     | otherwise     = fromLowAF w
{-# INLINE unsafeFromHexDigit #-}
