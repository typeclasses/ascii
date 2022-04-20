module ASCII.Hexadecimal
    (
    {- * Read/show for numeric strings -}
    {- ** Natural  -} showNatural,  readNatural,
    {- ** Integer  -} showInteger,  readInteger,
    {- ** Integral -} showIntegral, readIntegral,

    {- * Various digit types -} HexChar (..), HexLetter (..), HexCharBreakdown (..),

    {- * Monomorphic character conversions -}
    {- ** HexLetter ↔ D16              -} hexLetterD16, d16HexLetter,
    {- ** HexLetter ↔ HexChar          -} letterHexChar, hexCharLetter,
    {- ** HexChar   ↔ ASCII Char       -} hexAsciiChar, asciiCharHex,
    {- ** HexChar   ↔ D16              -} d16HexChar, hexCharD16,
    {- ** HexChar   ↔ HexCharBreakdown -} breakDownHexChar, assembleHexChar,

    {- * Hexadecimal character superset classes -}
    {- * Of hex character       -} HexCharSuperset (..),
    {- * Of hex character lists -} HexStringSuperset (..),

    {- * Character/number conversions -}
    {- ** Natural ↔ HexChar -} naturalHexCharMaybe, hexCharNatural, naturalHexCharUnsafe,
    {- ** Natural ↔ D16     -} naturalD16Maybe,     d16Natural,     naturalD16Unsafe,
    {- ** Integer ↔ HexChar -} integerHexCharMaybe, hexCharInteger, integerHexCharUnsafe,
    {- ** Integer ↔ D16     -} integerD16Maybe,     d16Integer,     integerD16Unsafe

    ) where

import ASCII.Case (Case (..))
import qualified ASCII.Char as ASCII
import qualified ASCII.Decimal as Dec
import ASCII.Lift (Lift (lift))
import ASCII.Refinement (ASCII, asciiUnsafe, lift)
import ASCII.Superset (StringSuperset, fromChar, fromCharList, toCharListMaybe,
                       toCharMaybe)
import ASCII.Word4 (Word4)
import qualified ASCII.Word4 as Word4

import Control.Monad (guard, (<=<), (=<<))
import Data.Bifoldable (bifoldMap)
import Data.Bits (Bits, toIntegralSized)
import Data.Bool (Bool, (&&))
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (id, ($), (.))
import Data.Functor (fmap)
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (Just, Nothing), fromJust, isJust)
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

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import qualified DList

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

---  Types  ---

-- | Letters used as hexadecimal digits above 9, without a notion of case.
data HexLetter =
    LetterA -- ^ 10
  | LetterB -- ^ 11
  | LetterC -- ^ 12
  | LetterD -- ^ 13
  | LetterE -- ^ 14
  | LetterF -- ^ 15
    deriving stock (Bounded, Enum, Eq, Ord, Show, Data, Generic)
    deriving anyclass Hashable

{- | The subset of ASCII used to represent hexadecimal numbers:

* 'ASCII.Char.Digit0' to 'ASCII.Char.Digit9'
* 'ASCII.Char.CapitalLetterA' to 'ASCII.Char.CapitalLetterF'
* 'ASCII.Char.SmallLetterA' to 'ASCII.Char.SmallLetterF'
-}
data HexChar =
      Digit0 | Digit1 | Digit2 | Digit3 | Digit4
    | Digit5 | Digit6 | Digit7 | Digit8 | Digit9
    | CapitalLetterA | CapitalLetterB | CapitalLetterC
    | CapitalLetterD | CapitalLetterE | CapitalLetterF
    | SmallLetterA | SmallLetterB | SmallLetterC
    | SmallLetterD | SmallLetterE | SmallLetterF
    deriving stock (Bounded, Enum, Eq, Ord, Show, Data, Generic)
    deriving anyclass Hashable

data HexCharBreakdown = HexDigit Dec.Digit | HexLetter Case HexLetter
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass Hashable

-- | Behaves the same as 'HexChar'
instance Enum HexCharBreakdown
  where
    fromEnum = fromEnum . assembleHexChar
    toEnum = breakDownHexChar . toEnum

-- | Behaves the same as 'HexChar'
instance Bounded HexCharBreakdown
  where
    minBound = breakDownHexChar minBound
    maxBound = breakDownHexChar maxBound


---  Monomorphic conversions between the character types  ---

breakDownHexChar :: HexChar -> HexCharBreakdown
breakDownHexChar =
    \case
        x | x <= Digit9          ->  HexDigit            (toEnum (fromEnum x))
        x | x <= CapitalLetterF  ->  HexLetter UpperCase (toEnum (fromEnum x - 10))
        x                        ->  HexLetter LowerCase (toEnum (fromEnum x - 16))

assembleHexChar :: HexCharBreakdown -> HexChar
assembleHexChar =
    \case
        HexDigit x             ->  toEnum (fromEnum x)
        HexLetter UpperCase x  ->  toEnum (fromEnum x + 10)
        HexLetter LowerCase x  ->  toEnum (fromEnum x + 16)

d16HexChar :: Case -> Word4 -> HexChar
d16HexChar c =
    toEnum
    .
    case c of
        UpperCase -> fromEnum
        LowerCase -> \case
            x | x <= Word4.Number9  ->  fromEnum x
            x                       ->  fromEnum x + 6

hexCharD16 :: HexChar -> Word4
hexCharD16 =
    \case
        x | x < SmallLetterA  ->  toEnum (fromEnum x)
        x                     ->  toEnum (fromEnum x - 6)

hexAsciiChar :: HexChar -> ASCII.Char
hexAsciiChar =
    toEnum
    .
    \case
        x | x <= Digit9          ->  fromEnum ASCII.Digit0         + fromEnum x
        x | x <= CapitalLetterF  ->  fromEnum ASCII.CapitalLetterA + fromEnum x - 10
        x                        ->  fromEnum ASCII.SmallLetterA   + fromEnum x - 16

asciiCharHex :: ASCII.Char -> Maybe HexChar
asciiCharHex =
    \case
        x | x >= ASCII.Digit0 && x <= ASCII.Digit9 ->
            Just $ toEnum $
                fromEnum x - fromEnum ASCII.Digit0

        x | x >= ASCII.CapitalLetterA && x <= ASCII.CapitalLetterF ->
            Just $ toEnum $
                fromEnum x + 10 - fromEnum ASCII.CapitalLetterA

        x | x >= ASCII.SmallLetterA && x <= ASCII.SmallLetterF ->
            Just $ toEnum $
                fromEnum x + 16 - fromEnum ASCII.SmallLetterA

        _ -> Nothing

hexLetterD16 :: HexLetter -> Word4
hexLetterD16 = toEnum . (\x -> x + 10) . fromEnum

d16HexLetter :: Word4 -> Maybe HexLetter
d16HexLetter x =
  do
    guard (x >= Word4.Number10)
    Just (toEnum (fromEnum x - 10))

letterHexChar :: Case -> HexLetter -> HexChar
letterHexChar = \case
    UpperCase -> toEnum . (\x -> x + 10) . fromEnum
    LowerCase -> toEnum . (\x -> x + 16) . fromEnum

hexCharLetter :: HexChar -> Maybe HexLetter
hexCharLetter = \case
    x | x <= Digit9          ->  Nothing
    x | x <= CapitalLetterF  ->  Just (toEnum (fromEnum x - 10))
    x                        ->  Just (toEnum (fromEnum x - 16))


---  Monomorphic character/number conversions  ---

naturalHexCharMaybe :: Case -> Natural -> Maybe HexChar
naturalHexCharMaybe =
    \case
        UpperCase -> \case
            x | x <= 15 -> Just (toEnum (fromIntegral x))
            _           -> Nothing
        LowerCase -> \case
            x | x <= 9  -> Just (toEnum (fromIntegral x))
            x | x <= 15 -> Just (toEnum (fromIntegral x + 6))
            _           -> Nothing

naturalHexCharUnsafe :: Case -> Natural -> HexChar
naturalHexCharUnsafe =
    \case
        UpperCase -> \x -> toEnum (fromIntegral x)
        LowerCase -> \case
            x | x <= 9  -> toEnum (fromIntegral x)
            x           -> toEnum (fromIntegral x + 6)

hexCharNatural :: HexChar -> Natural
hexCharNatural =
    fromIntegral
    .
    \case
        x | x > 15  ->  x - 6
        x           ->  x
    .
    fromEnum

integerHexCharMaybe :: Case -> Integer -> Maybe HexChar
integerHexCharMaybe =
    \case
        UpperCase -> \case
            x | x < 0   -> Nothing
            x | x <= 15 -> Just (toEnum (fromIntegral x))
            _           -> Nothing
        LowerCase -> \case
            x | x < 0   -> Nothing
            x | x <= 9  -> Just (toEnum (fromIntegral x))
            x | x <= 15 -> Just (toEnum (fromIntegral x + 6))
            _           -> Nothing

integerHexCharUnsafe :: Case -> Integer -> HexChar
integerHexCharUnsafe =
    \case
        UpperCase -> \x -> toEnum (fromIntegral x)
        LowerCase -> \case
            x | x <= 9  -> toEnum (fromIntegral x)
            x           -> toEnum (fromIntegral x + 6)

hexCharInteger :: HexChar -> Integer
hexCharInteger =
    fromIntegral
    .
    \case
        x | x > 15  ->  x - 6
        x           ->  x
    .
    fromEnum

naturalD16Maybe :: Natural -> Maybe Word4
naturalD16Maybe =
    \case
        x | x <= 15  ->  Just (toEnum (fromIntegral x))
        _            ->  Nothing

d16Natural :: Word4 -> Natural
d16Natural = fromIntegral . fromEnum

integerD16Maybe :: Integer -> Maybe Word4
integerD16Maybe =
    \case
        x | x < 0    ->  Nothing
        x | x <= 15  ->  Just (toEnum (fromInteger x))
        _            ->  Nothing

d16Integer :: Word4 -> Integer
d16Integer = toInteger . fromEnum

naturalD16Unsafe :: Natural -> Word4
naturalD16Unsafe = toEnum . fromIntegral

integerD16Unsafe :: Integer -> Word4
integerD16Unsafe = toEnum . fromIntegral


---  Lift instances  ---

instance HexCharSuperset char => Lift HexChar char
  where
    lift = fromHexChar

instance HexStringSuperset string => Lift [HexChar] string
  where
    lift = fromHexCharList



---  Classes  ---

class HexCharSuperset char
  where

    fromHexChar :: HexChar -> char

    isHexChar :: char -> Bool
    isHexChar = isJust . toHexCharMaybe

    toHexCharUnsafe :: char -> HexChar
    toHexCharUnsafe = fromJust . toHexCharMaybe

    toHexCharMaybe :: char -> Maybe HexChar
    toHexCharMaybe x = if isHexChar x then Just (toHexCharUnsafe x) else Nothing

    {-# minimal fromHexChar, ((isHexChar, toHexCharUnsafe) | toHexCharMaybe) #-}

class HexStringSuperset string
  where

    fromHexCharList :: [HexChar] -> string

    isHexString :: string -> Bool
    isHexString = isJust . toHexCharListMaybe

    toHexCharListUnsafe :: string -> [HexChar]
    toHexCharListUnsafe = fromJust . toHexCharListMaybe

    toHexCharListMaybe :: string -> Maybe [HexChar]
    toHexCharListMaybe x = if isHexString x then Just (toHexCharListUnsafe x) else Nothing

    {-# minimal fromHexCharList, ((isHexString, toHexCharListUnsafe) | toHexCharListMaybe) #-}


---  Show functions  ---

{- |

Examples:

* @showNatural LowerCase 12@ = @"c"@
* @showNatural UpperCase (256 + 12)@ = @"10C"@
* @showNatural UpperCase 0@ = @"0"@

-}
showNatural :: HexStringSuperset string => Case -> Natural -> string
showNatural =
    \c -> \case
        0 -> fromHexCharList [ Digit0 ]
        n -> fromHexCharList (fmap (d16HexChar c) (naturalList n))
  where
    naturalList :: Natural -> [Word4]
    naturalList = DList.toList . r
      where
        r = \case
            0 -> mempty
            n ->
                bifoldMap
                    r
                    (DList.singleton . naturalD16Unsafe)
                    (quotRem n 16)

{- |

Examples:

* @showInteger LowerCase 12@ = @"c"@
* @showInteger LowerCase (negate 12)@ = @"-c"@
* @showInteger UpperCase (256 + 12)@ = @"10C"@
* @showInteger UpperCase (negate (256 + 12))@ = @"-10C"@
* @showInteger UpperCase 0@ = @"0"@

-}
showInteger :: StringSuperset string => Case -> Integer -> string
showInteger = \c -> fromCharList . integerList c
  where
    integerList :: Case -> Integer -> [ASCII.Char]
    integerList c =
        \case
            0           ->  [ ASCII.Digit0 ]
            n | n < 0   ->  ASCII.HyphenMinus : nonNegativeIntegerList c (abs n)
            n           ->  nonNegativeIntegerList c n

    nonNegativeIntegerList :: Case -> Integer -> [ASCII.Char]
    nonNegativeIntegerList c = DList.toList . r
      where
        r = \case
            0 -> mempty
            n ->
                bifoldMap
                    r
                    (DList.singleton . fromHexChar . integerHexCharUnsafe c)
                    (quotRem n 16)

showIntegral :: (StringSuperset string, Integral number) => Case -> number -> string
showIntegral c = showInteger c . toInteger


---  Read functions  ---

{- |

Examples:

* @readNatural "5"@ = @Just 5@
* @readNatural "-5"@ = @Nothing@
* @readNatural "1f"@ = @Just 31@
* @readNatural "1F"@ = @Just 31@
* @readNatural "xa"@ = @Nothing@
* @readNatural ""@ = @Nothing@

-}
readNatural :: HexStringSuperset string => string -> Maybe Natural
readNatural = (Just . readNaturalDigits) <=< nonEmpty <=< (Just . fmap hexCharD16) <=< toHexCharListMaybe
  where
    readNaturalDigits :: NonEmpty Word4 -> Natural
    readNaturalDigits = List.foldl' (\total x -> (16 * total) + d16Natural x) 0

{- |

Examples:

* @readInteger "5"@ = @Just 5@
* @readInteger "-5"@ = @Just (-5)@
* @readInteger "1f"@ = @Just 31@
* @readInteger "1F"@ = @Just 31@
* @readInteger "xa"@ = @Nothing@
* @readInteger ""@ = @Nothing@
* @readInteger "-"@ = @Nothing@

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
    readNonNegative = (Just . toInteger . readIntegerDigits) <=< nonEmpty <=< (Just . fmap hexCharD16) <=< toHexCharListMaybe

    readIntegerDigits :: NonEmpty Word4 -> Integer
    readIntegerDigits = List.foldl' (\total x -> (16 * total) + d16Integer x) 0

{- |

Examples:

* @readIntegral "0014"@ = @Just (20 :: Word8)@
* @readIntegral ""@ = @(Nothing :: Maybe Word8)@
* @readIntegral "-4"@ = @(Nothing :: Maybe Word8)@
* @readIntegral "1234"@ = @(Nothing :: Maybe Word8)@

-}
readIntegral :: (StringSuperset string, Integral number, Bits number) => string -> Maybe number
readIntegral = toIntegralSized <=< readInteger


---  HexCharSuperset instances  ---

instance HexCharSuperset HexChar
  where
    isHexChar _ = Bool.True
    fromHexChar = id
    toHexCharUnsafe = id
    toHexCharMaybe = Just

instance HexCharSuperset ASCII.Char
  where
    fromHexChar = hexAsciiChar
    toHexCharMaybe = asciiCharHex

instance HexCharSuperset Unicode.Char
  where
    fromHexChar = fromChar . fromHexChar
    toHexCharMaybe = toHexCharMaybe <=< toCharMaybe

instance HexCharSuperset Word8
  where
    fromHexChar x = fromChar (fromHexChar x :: ASCII.Char)
    toHexCharMaybe w = toHexCharMaybe =<< (toCharMaybe w :: Maybe ASCII.Char)

instance HexCharSuperset char => HexCharSuperset (ASCII char)
  where
    isHexChar = isHexChar . ASCII.Refinement.lift
    fromHexChar = asciiUnsafe . fromHexChar
    toHexCharUnsafe = toHexCharUnsafe . ASCII.Refinement.lift
    toHexCharMaybe = toHexCharMaybe . ASCII.Refinement.lift


---  HexStringSuperset instances  ---

instance HexStringSuperset [HexChar]
  where
    fromHexCharList = id
    isHexString _ = Bool.True
    toHexCharListUnsafe = id
    toHexCharListMaybe = Just

instance HexStringSuperset [ASCII.Char]
  where
    fromHexCharList = List.map fromHexChar
    isHexString = List.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe

instance HexStringSuperset [Unicode.Char]
  where
    fromHexCharList = List.map fromHexChar
    isHexString = List.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe

instance HexStringSuperset T.Text
  where
    fromHexCharList = T.pack . List.map fromHexChar
    isHexString = T.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe . T.unpack

instance HexStringSuperset LT.Text
  where
    fromHexCharList = LT.pack . List.map fromHexChar
    isHexString = LT.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe . LT.unpack

instance HexStringSuperset TB.Builder
  where
    fromHexCharList = TB.fromLazyText . fromHexCharList
    toHexCharListMaybe = toHexCharListMaybe . TB.toLazyText

instance HexStringSuperset BS.ByteString
  where
    fromHexCharList = BS.pack . List.map fromHexChar
    isHexString = BS.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe . BS.unpack

instance HexStringSuperset LBS.ByteString
  where
    fromHexCharList = LBS.pack . List.map fromHexChar
    isHexString = LBS.all isHexChar
    toHexCharListUnsafe = List.map toHexCharUnsafe . LBS.unpack

instance HexStringSuperset BSB.Builder
  where
    fromHexCharList = BSB.lazyByteString . fromHexCharList
    toHexCharListMaybe = toHexCharListMaybe . BSB.toLazyByteString

instance HexStringSuperset char => HexStringSuperset (ASCII char)
  where
    isHexString = isHexString . ASCII.Refinement.lift
    fromHexCharList = asciiUnsafe . fromHexCharList
    toHexCharListUnsafe = toHexCharListUnsafe . ASCII.Refinement.lift
    toHexCharListMaybe = toHexCharListMaybe . ASCII.Refinement.lift
