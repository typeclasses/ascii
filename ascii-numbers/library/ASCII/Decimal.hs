module ASCII.Decimal
    (
    {- * Read/show for numeric strings -}
    {- ** Natural -} showNatural, readNatural,
    {- ** Integer -} showInteger, readInteger,
    {- ** Integral -} showIntegral, readIntegral,

    {- * The digit type -} D10 (..),

    {- * Decimal digit superset classes -}
    {- ** Of digit -} DigitSuperset (..),
    {- ** Of digit lists -} DigitStringSuperset (..),

    {- * Character/number conversions -}
    {- ** Natural -} naturalDigitMaybe, digitNatural,
    {- ** Integer -} integerDigitMaybe, digitInteger

    ) where

import qualified ASCII.Char as ASCII
import ASCII.Refinement (ASCII, asciiUnsafe, lift)
import ASCII.Superset (StringSuperset, fromChar, fromCharList, toCharListMaybe,
                       toCharMaybe)

import Control.Monad ((<=<), (=<<))
import Data.Bifoldable (bifoldMap)
import Data.Bits (Bits, toIntegralSized)
import Data.Bool (Bool, (&&))
import Data.Function (id, (.))
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (Maybe (..), fromJust, isJust)
import Data.Monoid (mempty)
import Data.Ord (Ord (..))
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Prelude (Integer, Integral, abs, fromEnum, fromInteger, fromIntegral,
                negate, quotRem, toEnum, toInteger, (*), (+), (-))

import qualified Data.Bool as Bool
import qualified Data.Char as Unicode
import qualified Data.List as List

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import DList (DList)
import qualified DList

import D10.Safe (D10 (..))
import qualified D10.Safe as D10


---  Show functions  ---

{- |

Examples:

* @showNatural 0@ = @"0"@
* @showNatural 268@ = @"268"@

-}
showNatural :: DigitStringSuperset string => Natural -> string
showNatural =
    \case
        0 -> fromDigitList [ D0 ]
        n -> fromDigitList (naturalList n)
  where
    naturalList :: Natural -> [D10]
    naturalList = DList.toList . r
      where
        r :: Natural -> DList D10
        r = \case
            0 -> mempty
            n ->
                bifoldMap
                    r
                    (DList.singleton . toEnum . fromIntegral)
                    (quotRem n 10)

{- |

Examples:

* @showInteger 0@ = @"0"@
* @showInteger 12@ = @"12"@
* @showInteger (negate 12)@ = @"-12"@

-}
showInteger :: StringSuperset string => Integer -> string
showInteger = fromCharList . integerList
  where
    integerList :: Integer -> [ASCII.Char]
    integerList =
        \case
            0           ->  [ ASCII.Digit0 ]
            n | n < 0   ->  ASCII.HyphenMinus : nonNegativeIntegerList (abs n)
            n           ->  nonNegativeIntegerList n

    nonNegativeIntegerList :: Integer -> [ASCII.Char]
    nonNegativeIntegerList = DList.toList . r
      where
        r :: Integer -> DList ASCII.Char
        r = \case
            0 -> mempty
            n ->
                bifoldMap
                    r
                    (DList.singleton . fromDigit . toEnum . fromInteger)
                    (quotRem n 10)

showIntegral :: (Integral n, StringSuperset string) => n -> string
showIntegral = showInteger . toInteger


---  Read functions  ---

{- |

Examples:

* @readNatural "0"@ = @Just 0@
* @readNatural "268"@ = @Just 268@
* @readNatural "0004"@ = @Just 4@
* @readNatural ""@ = @Nothing@
* @readNatural "-4"@ = @Nothing@
* @readNatural "12345678901234567890"@ = @Just 12345678901234567890@

-}
readNatural :: DigitStringSuperset string => string -> Maybe Natural
readNatural = (Just . readNaturalDigits) <=< nonEmpty <=< toDigitListMaybe
  where
    readNaturalDigits :: NonEmpty D10 -> Natural
    readNaturalDigits = List.foldl' (\total x -> (10 * total) + fromIntegral (fromEnum x)) 0

{- |

Examples:

* @readInteger "0"@ = @Just 0@
* @readInteger "268"@ = @Just 268@
* @readInteger "0004"@ = @Just 4@
* @readInteger ""@ = @Nothing@
* @readInteger "-4"@ = @Just (-4)@
* @readInteger "12345678901234567890"@ = @Just 12345678901234567890@

-}
readInteger :: StringSuperset string => string -> Maybe Integer
readInteger = readIntegerCharList <=< toCharListMaybe
  where
    readIntegerCharList :: [ASCII.Char] -> Maybe Integer
    readIntegerCharList =
        \case
            ASCII.HyphenMinus : xs  ->  fmap negate (readNonNegative xs)
            xs                      ->  readNonNegative xs

    readNonNegative :: [ASCII.Char] -> Maybe Integer
    readNonNegative = (Just . toInteger . readIntegerDigits) <=< nonEmpty <=< toDigitListMaybe

    readIntegerDigits :: NonEmpty D10 -> Integer
    readIntegerDigits = List.foldl' (\total x -> (10 * total) + fromIntegral (fromEnum x)) 0

{- |

Examples:

* @readIntegral "0"@ = @Just (0 :: Word8)@
* @readIntegral "175"@ = @Just (175 :: Word8)@
* @readIntegral "268"@ = @(Nothing :: Maybe Word8)@
* @readIntegral "0004"@ = @Just (4 :: Word8)@
* @readIntegral ""@ = @(Nothing :: Maybe Word8)@
* @readIntegral "-4"@ = @(Nothing :: Maybe Word8)@
* @readIntegral "12345678901234567890"@ = @(Nothing :: Maybe Word8)@

-}
readIntegral :: (StringSuperset string, Integral num, Bits num) => string -> Maybe num
readIntegral = toIntegralSized <=< readInteger


---  Uninteresting monomorphic specializations of polymorphic functions  ---

{- |

Examples:

* @naturalDigitMaybe 5@ = @Just Digit5@
* @naturalDigitMaybe 12@ = @Nothing@

-}
naturalDigitMaybe :: Natural -> Maybe D10
naturalDigitMaybe = D10.natD10Maybe

{- |

Examples:

* @integerDigitMaybe 5@ = @Just Digit5@
* @integerDigitMaybe 12@ = @Nothing@

-}
integerDigitMaybe :: Integer -> Maybe D10
integerDigitMaybe = D10.integerD10Maybe

digitNatural :: D10 -> Natural
digitNatural = D10.d10Nat

digitInteger :: D10 -> Integer
digitInteger = D10.d10Integer


---  Classes  ---

class DigitSuperset char
  where

    fromDigit :: D10 -> char

    isDigit :: char -> Bool
    isDigit = isJust . toDigitMaybe

    toDigitUnsafe :: char -> D10
    toDigitUnsafe = fromJust . toDigitMaybe

    toDigitMaybe :: char -> Maybe D10
    toDigitMaybe x = if isDigit x then Just (toDigitUnsafe x) else Nothing

    {-# minimal fromDigit, ((isDigit, toDigitUnsafe) | toDigitMaybe) #-}

class DigitStringSuperset string
  where

    fromDigitList :: [D10] -> string

    isDigitString :: string -> Bool
    isDigitString = isJust . toDigitListMaybe

    toDigitListUnsafe :: string -> [D10]
    toDigitListUnsafe = fromJust . toDigitListMaybe

    toDigitListMaybe :: string -> Maybe [D10]
    toDigitListMaybe x = if isDigitString x then Just (toDigitListUnsafe x) else Nothing

    {-# minimal fromDigitList, ((isDigitString, toDigitListUnsafe) | toDigitListMaybe) #-}


---  DigitSuperset instances  ---

instance DigitSuperset D10
  where
    isDigit _ = Bool.True
    fromDigit = id
    toDigitUnsafe = id
    toDigitMaybe = Just

instance DigitSuperset ASCII.Char
  where
    isDigit x = x >= ASCII.Digit0 && x <= ASCII.Digit9
    fromDigit     = toEnum . (\x -> x + fromEnum ASCII.Digit0) . fromEnum
    toDigitUnsafe = toEnum . (\x -> x - fromEnum ASCII.Digit0) . fromEnum

instance DigitSuperset Unicode.Char
  where
    isDigit x = x >= '0' && x <= '9'
    fromDigit     = Unicode.chr . (\x -> x + Unicode.ord '0') . fromEnum
    toDigitUnsafe = toEnum      . (\x -> x - Unicode.ord '0') . Unicode.ord

instance DigitSuperset Word8
  where
    fromDigit x = fromChar (fromDigit x :: ASCII.Char)
    toDigitMaybe w = toDigitMaybe =<< (toCharMaybe w :: Maybe ASCII.Char)

instance DigitSuperset char => DigitSuperset (ASCII char)
  where
    isDigit = isDigit . lift
    fromDigit = asciiUnsafe . fromDigit
    toDigitUnsafe = toDigitUnsafe . lift
    toDigitMaybe = toDigitMaybe . lift


---  DigitStringSuperset instances  ---

instance DigitStringSuperset [D10]
  where
    isDigitString _ = Bool.True
    fromDigitList = id
    toDigitListUnsafe = id
    toDigitListMaybe = Just

instance DigitStringSuperset [ASCII.Char]
  where
    isDigitString = List.all isDigit
    fromDigitList = List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe

instance DigitStringSuperset [Unicode.Char]
  where
    isDigitString = List.all isDigit
    fromDigitList = List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe

instance DigitStringSuperset T.Text
  where
    isDigitString = T.all isDigit
    fromDigitList = T.pack . List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe . T.unpack

instance DigitStringSuperset LT.Text
  where
    isDigitString = LT.all isDigit
    fromDigitList = LT.pack . List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe . LT.unpack

instance DigitStringSuperset TB.Builder
  where
    fromDigitList = TB.fromLazyText . fromDigitList
    toDigitListMaybe = toDigitListMaybe . TB.toLazyText

instance DigitStringSuperset BS.ByteString
  where
    isDigitString = BS.all isDigit
    fromDigitList = BS.pack . List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe . BS.unpack

instance DigitStringSuperset LBS.ByteString
  where
    isDigitString = LBS.all isDigit
    fromDigitList = LBS.pack . List.map fromDigit
    toDigitListUnsafe = List.map toDigitUnsafe . LBS.unpack

instance DigitStringSuperset BSB.Builder
  where
    fromDigitList = BSB.lazyByteString . fromDigitList
    toDigitListMaybe = toDigitListMaybe . BSB.toLazyByteString

instance DigitStringSuperset char => DigitStringSuperset (ASCII char)
  where
    isDigitString = isDigitString . lift
    fromDigitList = asciiUnsafe . fromDigitList
    toDigitListUnsafe = toDigitListUnsafe . lift
    toDigitListMaybe = toDigitListMaybe . lift
