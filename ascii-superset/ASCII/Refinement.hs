module ASCII.Refinement
  (
  -- * ASCII type constructor
    ASCII, lift, asciiUnsafe

  -- * Character functions
  , validateChar, fromChar, toChar, substituteChar, toCharSub

  -- * String functions
  , fromCharList, toCharList

  ) where

import qualified ASCII.Char as ASCII
import qualified ASCII.Superset as S

import Data.Function ((.))
import Data.Maybe (Maybe (..))

newtype ASCII a = ASCII_Unsafe { lift :: a }

asciiUnsafe :: a -> ASCII a
asciiUnsafe = ASCII_Unsafe

validateChar :: S.AsciiCharSuperset a => a -> Maybe (ASCII a)
validateChar x = if S.isAsciiChar x then Just (asciiUnsafe x) else Nothing

substituteChar :: S.AsciiCharSuperset a => a -> ASCII a
substituteChar x = if S.isAsciiChar x then asciiUnsafe x else fromChar ASCII.Substitute

fromChar :: S.AsciiCharSuperset a => ASCII.Char -> ASCII a
fromChar = asciiUnsafe . S.fromChar

toChar :: S.AsciiCharSuperset a => ASCII a -> ASCII.Char
toChar = S.toCharUnsafe . lift

toCharSub :: S.AsciiCharSuperset a => a -> ASCII.Char
toCharSub x = if S.isAsciiChar x then S.toCharUnsafe x else ASCII.Substitute

fromCharList :: S.AsciiStringSuperset a => [ASCII.Char] -> ASCII a
fromCharList = asciiUnsafe . S.fromCharList

toCharList :: S.AsciiStringSuperset a => ASCII a -> [ASCII.Char]
toCharList = S.toCharListUnsafe . lift
