module ASCII.Isomorphism ( CharIso (..), asChar, StringIso (..) ) where

import qualified ASCII.Char as ASCII
import ASCII.Superset
import Data.Function (id, (.))
import qualified Data.List as List

class CharSuperset char => CharIso char
  where
    toChar :: char -> ASCII.Char

asChar :: CharIso char => (ASCII.Char -> ASCII.Char) -> char -> char
asChar f = fromChar . f . toChar

class StringSuperset string => StringIso string
  where
    toCharList :: string -> [ASCII.Char]
    mapChars :: (ASCII.Char -> ASCII.Char) -> string -> string

-- | 'ASCII.Char' is trivially isomorphic to itself.
instance CharIso ASCII.Char
  where
    toChar = id

instance CharIso char => StringIso [char]
  where
    toCharList = List.map toChar
    mapChars f = List.map (asChar f)
