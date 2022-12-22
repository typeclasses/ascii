module ASCII.Refinement
  (
    {- * ASCII type constructor -} ASCII, lift, asciiUnsafe,
    {- * Character functions -} validateChar, fromChar, toChar, substituteChar, asChar,
    {- * String functions -} validateString, fromCharList, toCharList, substituteString, mapChars
  )
  where

import qualified ASCII.Char as ASCII
import qualified ASCII.Isomorphism as I
import qualified ASCII.Superset as S

import ASCII.Superset (CharSuperset, StringSuperset)
import Data.Bool (Bool (..))
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (id, ($), (.))
import Data.Hashable (Hashable)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>))
import Data.Semigroup (Semigroup)
import GHC.Generics (Generic)
import Prelude (succ)
import Text.Show (Show, showList, showParen, showString, showsPrec)

{- | This type constructor indicates that a value from some ASCII superset is valid ASCII. The type parameter is the ASCII superset, which should be a type with an instance of either 'CharSuperset' or 'StringSuperset'.

For example, whereas a 'Data.Text.Text' value may contain a combination of ASCII and non-ASCII characters, a value of type @'ASCII' 'Data.Text.Text'@ may contain only ASCII characters.

-}

newtype ASCII superset = ASCII_Unsafe
  { lift :: superset
      -- ^ Discard the evidence that the value is known to consist entirely of ASCII characters
  }

deriving stock instance Eq superset => Eq (ASCII superset)

deriving stock instance Ord superset => Ord (ASCII superset)

deriving newtype instance Hashable superset => Hashable (ASCII superset)

deriving newtype instance Semigroup superset => Semigroup (ASCII superset)

deriving newtype instance Monoid superset => Monoid (ASCII superset)

deriving stock instance Data superset => Data (ASCII superset)

deriving stock instance Generic (ASCII superset)

instance Show superset => Show (ASCII superset)
  where
    showsPrec d x = showParen (d > app_prec) $
        showString "asciiUnsafe " . showsPrec (succ app_prec) (lift x)
      where app_prec = 10

    showList x = showString "asciiUnsafe " . showList (map lift x)

instance CharSuperset char => CharSuperset (ASCII char)
  where
    isAsciiChar _ = True
    fromChar = asciiUnsafe . S.fromChar
    toCharUnsafe = S.toCharUnsafe . lift

instance CharSuperset char => I.CharIso (ASCII char)
  where
    toChar = S.toCharUnsafe

instance StringSuperset string => StringSuperset (ASCII string)
  where
    isAsciiString _ = True
    fromCharList = asciiUnsafe . S.fromCharList
    toCharListUnsafe = S.toCharListUnsafe . lift
    toCharListSub = S.toCharListUnsafe . lift
    substituteString = id

instance StringSuperset string => I.StringIso (ASCII string)
  where
    toCharList = S.toCharListUnsafe
    mapChars = S.mapCharsUnsafe

-- | Unchecked conversion to ASCII; undefined if the superset contains non-ASCII characters
asciiUnsafe :: superset -> ASCII superset
asciiUnsafe = ASCII_Unsafe

{- | Return 'Just' an 'ASCII' character if the input is an ASCII character, or 'Nothing' otherwise

>>> map validateChar [-1, 65, 97, 128] :: [Maybe (ASCII Int)]
[Nothing,Just (asciiUnsafe 65),Just (asciiUnsafe 97),Nothing]

-}

validateChar :: CharSuperset superset => superset -> Maybe (ASCII superset)
validateChar x = if S.isAsciiChar x then Just (asciiUnsafe x) else Nothing

-- | Return an 'ASCII' character if the input is an ASCII character, or 'ASCII.Substitute' otherwise
substituteChar :: CharSuperset superset => superset -> ASCII superset
substituteChar x = if S.isAsciiChar x then asciiUnsafe x else fromChar ASCII.Substitute

-- | Lift an ASCII 'ASCII.Char' into a superset type, wrapped in the 'ASCII' refinement to save the evidence that it is ASCII
fromChar :: CharSuperset superset => ASCII.Char -> ASCII superset
fromChar = asciiUnsafe . S.fromChar

-- | Given a character from some type that is known to represent an ASCII character, obtain the ASCII character it represents
toChar :: CharSuperset superset => ASCII superset -> ASCII.Char
toChar = S.toCharUnsafe . lift

{- | Lift a list of ASCII 'ASCII.Char's into a superset string type, wrapped in the 'ASCII' refinement to save the evidence that all of the characters in the string are ASCII.

>>> fromCharList [CapitalLetterH,SmallLetterI,ExclamationMark] :: ASCII Text
asciiUnsafe "Hi!"

-}

fromCharList :: StringSuperset superset => [ASCII.Char] -> ASCII superset
fromCharList = asciiUnsafe . S.fromCharList

{- | Given a string from some type that is known to represent only ASCII characters, obtain the ASCII characters it represents

>>> toCharList (substituteString "Piñata" :: ASCII Text)
[CapitalLetterP,SmallLetterI,Substitute,SmallLetterA,SmallLetterT,SmallLetterA]

-}

toCharList :: StringSuperset superset => ASCII superset -> [ASCII.Char]
toCharList = S.toCharListUnsafe . lift

{- | Forces a string from a larger character set into ASCII by using the 'ASCII.Substitute' character in place of any non-ASCII characters.

>>> substituteString "Cristóbal" :: ASCII Text
asciiUnsafe "Crist\SUBbal"

-}

substituteString :: StringSuperset superset => superset -> ASCII superset
substituteString = asciiUnsafe . S.substituteString

{- | Return 'Just' an 'ASCII' string if the input consists entirely of ASCII characters, or 'Nothing' otherwise

>>> map validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII Text)]
[Just (asciiUnsafe "Hello"),Nothing]

>>> map validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII String)]
[Just (asciiUnsafe "Hello"),Nothing]

-}

validateString :: StringSuperset superset => superset -> Maybe (ASCII superset)
validateString x = if S.isAsciiString x then Just (asciiUnsafe x) else Nothing

-- | Given a character from a larger set that is known to represent an ASCII character, manipulate it as if it were an ASCII character
asChar :: CharSuperset superset => (ASCII.Char -> ASCII.Char) -> ASCII superset -> ASCII superset
asChar f = asciiUnsafe . S.asCharUnsafe f . lift

-- | Given a string from a larger set that is known to consist entirely of ASCII characters, map over the characters in the string as if they were ASCII characters
mapChars :: StringSuperset superset => (ASCII.Char -> ASCII.Char) -> ASCII superset -> ASCII superset
mapChars f = asciiUnsafe . S.mapCharsUnsafe f . lift
