module ASCII.Decimal
    (
    {- * Read/show for numeric strings -}
    {- ** Natural -} showNatural, readNatural,
    {- ** Integer -} showInteger, readInteger,
    {- ** Integral -} showIntegral, readIntegral,

    {- * The digit type -} Digit (..),

    {- * Decimal digit superset classes -}
    {- ** Of digit -} DigitSuperset (..),
    {- ** Of digit lists -} DigitStringSuperset (..),

    {- * Character/number conversions -}
    {- ** Natural -} naturalDigitMaybe, digitNatural,
    {- ** Integer -} integerDigitMaybe, digitInteger

    ) where

import qualified ASCII.Char as ASCII
import qualified ASCII.Refinement
import ASCII.Lift (Lift (lift))
import ASCII.Refinement (ASCII, asciiUnsafe)
import ASCII.Superset (StringSuperset, fromChar, fromCharList, toCharListMaybe,
                       toCharMaybe)

import Control.Monad ((<=<), (=<<))
import Data.Bifoldable (bifoldMap)
import Data.Bits (Bits, toIntegralSized)
import Data.Bool (Bool, (&&), (||))
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (id, (.))
import Data.Functor (fmap)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (Maybe (..), fromJust, isJust)
import Data.Monoid (mempty)
import Data.Ord (Ord (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude (Bounded (..), Enum (..), Integer, Integral, abs, fromEnum,
                fromInteger, fromIntegral, negate, quotRem, toEnum, toInteger,
                (*), (+), (-))
import Text.Show (Show)

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

{- |

The subset of ASCII used to represent unsigned decimal numbers:

* 'ASCII.Char.Digit0' to 'ASCII.Char.Digit9'

-}
data Digit
    = Digit0  -- ^ Zero
    | Digit1  -- ^ One
    | Digit2  -- ^ Two
    | Digit3  -- ^ Three
    | Digit4  -- ^ Four
    | Digit5  -- ^ Five
    | Digit6  -- ^ Six
    | Digit7  -- ^ Seven
    | Digit8  -- ^ Eight
    | Digit9  -- ^ Nine
    deriving stock (Bounded, Enum, Eq, Ord, Show, Data, Generic)
    deriving anyclass Hashable


---  Show functions  ---

{- |

Examples:

* @showNatural 0@ = @"0"@
* @showNatural 268@ = @"268"@

-}
showNatural :: DigitStringSuperset string => Natural -> string
showNatural =
    \case
        0 -> fromDigitList [ Digit0 ]
        n -> fromDigitList (naturalList n)
  where
    naturalList :: Natural -> [Digit]
    naturalList = DList.toList . r
      where
        r :: Natural -> DList Digit
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
    readNaturalDigits :: NonEmpty Digit -> Natural
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

    readIntegerDigits :: NonEmpty Digit -> Integer
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
naturalDigitMaybe :: Natural -> Maybe Digit
naturalDigitMaybe n = if n > 9 then Nothing else Just (toEnum (fromIntegral n))

{- |

Examples:

* @integerDigitMaybe 5@ = @Just Digit5@
* @integerDigitMaybe 12@ = @Nothing@

-}
integerDigitMaybe :: Integer -> Maybe Digit
integerDigitMaybe n = if (n < 0 || n > 9) then Nothing else Just (toEnum (fromInteger n))

digitNatural :: Digit -> Natural
digitNatural = fromIntegral . fromEnum

digitInteger :: Digit -> Integer
digitInteger = fromIntegral . fromEnum


---  Lift instances  ---

instance DigitSuperset char => Lift Digit char
  where
    lift = fromDigit

instance DigitStringSuperset string => Lift [Digit] string
  where
    lift = fromDigitList


---  Classes  ---

class DigitSuperset char
  where

    fromDigit :: Digit -> char

    isDigit :: char -> Bool
    isDigit = isJust . toDigitMaybe

    toDigitUnsafe :: char -> Digit
    toDigitUnsafe = fromJust . toDigitMaybe

    toDigitMaybe :: char -> Maybe Digit
    toDigitMaybe x = if isDigit x then Just (toDigitUnsafe x) else Nothing

    {-# minimal fromDigit, ((isDigit, toDigitUnsafe) | toDigitMaybe) #-}

class DigitStringSuperset string
  where

    fromDigitList :: [Digit] -> string

    isDigitString :: string -> Bool
    isDigitString = isJust . toDigitListMaybe

    toDigitListUnsafe :: string -> [Digit]
    toDigitListUnsafe = fromJust . toDigitListMaybe

    toDigitListMaybe :: string -> Maybe [Digit]
    toDigitListMaybe x = if isDigitString x then Just (toDigitListUnsafe x) else Nothing

    {-# minimal fromDigitList, ((isDigitString, toDigitListUnsafe) | toDigitListMaybe) #-}


---  DigitSuperset instances  ---

instance DigitSuperset Digit
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
    isDigit = isDigit . ASCII.Refinement.lift
    fromDigit = asciiUnsafe . fromDigit
    toDigitUnsafe = toDigitUnsafe . ASCII.Refinement.lift
    toDigitMaybe = toDigitMaybe . ASCII.Refinement.lift


---  DigitStringSuperset instances  ---

instance DigitStringSuperset [Digit]
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
    isDigitString = isDigitString . ASCII.Refinement.lift
    fromDigitList = asciiUnsafe . fromDigitList
    toDigitListUnsafe = toDigitListUnsafe . ASCII.Refinement.lift
    toDigitListMaybe = toDigitListMaybe . ASCII.Refinement.lift
