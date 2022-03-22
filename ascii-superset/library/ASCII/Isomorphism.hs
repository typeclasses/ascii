module ASCII.Isomorphism ( CharIso (..), asChar, StringIso (..) ) where

import ASCII.Char (Char)
import ASCII.Superset (CharSuperset (..), StringSuperset (..))
import Data.Function (id, (.))
import Data.List (map)

class CharSuperset char => CharIso char
  where
    toChar :: char -> Char

asChar :: CharIso char => (Char -> Char) -> char -> char
asChar f = fromChar . f . toChar

class StringSuperset string => StringIso string
  where
    toCharList :: string -> [Char]
    mapChars :: (Char -> Char) -> string -> string

-- | 'Char' is trivially isomorphic to itself. (This instance is uninteresting.)
instance CharIso Char
  where
    toChar = id

instance CharIso char => StringIso [char]
  where
    toCharList = map toChar
    mapChars f = map (asChar f)
