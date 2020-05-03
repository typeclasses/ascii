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

import qualified Data.List as List
import qualified Text.Show as Show

import Data.Bool     ( Bool (..) )
import Data.Eq       ( Eq )
import Data.Function ( (.), ($), id )
import Data.Hashable ( Hashable )
import Data.Maybe    ( Maybe (..) )
import Data.Ord      ( Ord, (>) )
import Data.Text     ( Text )
import Prelude       ( succ )

{- $setup

>>> :set -XOverloadedStrings
>>> import ASCII.Char (Char (..))
>>> import Data.List (map)
>>> import Data.Int (Int)
>>> import Data.String (String)

-}

{- | This type constructor indicates that a value from some ASCII superset is valid ASCII. The type parameter is the ASCII superset, which should be a type with an instance of either 'S.IsChar' or 'S.IsString'.

For example, whereas a 'Text' value may contain a combination of ASCII and non-ASCII characters, a value of type @'ASCII' 'Text'@ may contain only ASCII characters.

-}

newtype ASCII superset = ASCII_Unsafe { lift :: superset }

deriving stock instance Eq superset => Eq (ASCII superset)
deriving stock instance Ord superset => Ord (ASCII superset)

deriving newtype instance Hashable superset => Hashable (ASCII superset)

instance Show.Show superset => Show.Show (ASCII superset)
  where
    showsPrec d x = Show.showParen (d > app_prec) $
        Show.showString "asciiUnsafe " . Show.showsPrec (succ app_prec) (lift x)
      where app_prec = 10

    showList x = Show.showString "asciiUnsafe " . Show.showList (List.map lift x)

instance S.IsChar superset => S.IsChar (ASCII superset)
  where
    isAsciiChar _ = True
    fromChar = asciiUnsafe . S.fromChar
    toCharUnsafe = S.toCharUnsafe . lift

instance S.IsString superset => S.IsString (ASCII superset)
  where
    isAsciiString _ = True
    fromCharList = asciiUnsafe . S.fromCharList
    toCharListUnsafe = S.toCharListUnsafe . lift
    toCharListSub = S.toCharListUnsafe . lift
    substituteString = id

asciiUnsafe :: superset -> ASCII superset
asciiUnsafe = ASCII_Unsafe

{- |

>>> map validateChar [-1, 65, 97, 128] :: [Maybe (ASCII Int)]
[Nothing,Just (asciiUnsafe 65),Just (asciiUnsafe 97),Nothing]

-}

validateChar :: S.IsChar superset => superset -> Maybe (ASCII superset)
validateChar x = if S.isAsciiChar x then Just (asciiUnsafe x) else Nothing

substituteChar :: S.IsChar superset => superset -> ASCII superset
substituteChar x = if S.isAsciiChar x then asciiUnsafe x else fromChar ASCII.Substitute

fromChar :: S.IsChar superset => ASCII.Char -> ASCII superset
fromChar = asciiUnsafe . S.fromChar

toChar :: S.IsChar superset => ASCII superset -> ASCII.Char
toChar = S.toCharUnsafe . lift

{- |

>>> fromCharList [CapitalLetterH,SmallLetterI,ExclamationMark] :: ASCII Text
asciiUnsafe "Hi!"

-}

fromCharList :: S.IsString superset => [ASCII.Char] -> ASCII superset
fromCharList = asciiUnsafe . S.fromCharList

{- |

>>> toCharList (substituteString "Piñata" :: ASCII Text)
[CapitalLetterP,SmallLetterI,Substitute,SmallLetterA,SmallLetterT,SmallLetterA]

-}

toCharList :: S.IsString superset => ASCII superset -> [ASCII.Char]
toCharList = S.toCharListUnsafe . lift

{- | Forces a string from a larger character set into ASCII by using the 'ASCII.Substitute' character in place of any non-ASCII characters.

>>> substituteString "Cristóbal" :: ASCII Text
asciiUnsafe "Crist\SUBbal"

-}

substituteString :: S.IsString superset => superset -> ASCII superset
substituteString = asciiUnsafe . S.substituteString

{- |

>>> map validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII Text)]
[Just (asciiUnsafe "Hello"),Nothing]

>>> map validateString ["Hello", "Cristóbal"] :: [Maybe (ASCII String)]
[Just (asciiUnsafe "Hello"),Nothing]

-}

validateString :: S.IsString superset => superset -> Maybe (ASCII superset)
validateString x = if S.isAsciiString x then Just (asciiUnsafe x) else Nothing
