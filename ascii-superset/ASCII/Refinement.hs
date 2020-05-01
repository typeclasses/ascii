module ASCII.Refinement
  (
  -- * ASCII type constructor
    ASCII, lift, asciiUnsafe

  -- * Character functions
  , validateChar, fromChar, toChar, substituteChar

  -- * String functions
  , validateString, fromCharList, toCharList, substituteString

  ) where

import qualified ASCII.Char as ASCII
import qualified ASCII.Superset as S

import qualified Text.Show as Show

import Data.Eq (Eq)
import Data.Function ((.), ($))
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord, (>))
import Data.Text (Text)
import Prelude (succ)

{- $setup

>>> :set -XOverloadedStrings
>>> import Prelude (map)

-}

{- | This type constructor indicates that a value from some ASCII superset is valid ASCII. The type parameter is the ASCII superset, which should be a type with an instance of either 'S.IsChar' or 'S.IsString'.

For example, whereas a 'Text' value may contain a combination of ASCII and non-ASCII characters, a value of type @'ASCII' 'Text'@ may contain only ASCII characters.

>>> map validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII Text)]
[Just (asciiUnsafe "Hello"),Nothing]

-}

newtype ASCII a = ASCII_Unsafe { lift :: a }

deriving stock instance Eq a => Eq (ASCII a)
deriving stock instance Ord a => Ord (ASCII a)

deriving newtype instance Hashable a => Hashable (ASCII a)

instance Show.Show a => Show.Show (ASCII a)
  where
    showsPrec d x = Show.showParen (d > app_prec) $
        Show.showString "asciiUnsafe " . Show.showsPrec (succ app_prec) (lift x)
      where app_prec = 10

asciiUnsafe :: a -> ASCII a
asciiUnsafe = ASCII_Unsafe

validateChar :: S.IsChar a => a -> Maybe (ASCII a)
validateChar x = if S.isAsciiChar x then Just (asciiUnsafe x) else Nothing

substituteChar :: S.IsChar a => a -> ASCII a
substituteChar x = if S.isAsciiChar x then asciiUnsafe x else fromChar ASCII.Substitute

fromChar :: S.IsChar a => ASCII.Char -> ASCII a
fromChar = asciiUnsafe . S.fromChar

toChar :: S.IsChar a => ASCII a -> ASCII.Char
toChar = S.toCharUnsafe . lift

fromCharList :: S.IsString a => [ASCII.Char] -> ASCII a
fromCharList = asciiUnsafe . S.fromCharList

toCharList :: S.IsString a => ASCII a -> [ASCII.Char]
toCharList = S.toCharListUnsafe . lift

substituteString :: S.IsString a => a -> ASCII a
substituteString = asciiUnsafe . S.substituteString

validateString :: S.IsString a => a -> Maybe (ASCII a)
validateString x = if S.isAsciiString x then Just (asciiUnsafe x) else Nothing
