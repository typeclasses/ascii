{-# LANGUAGE FlexibleInstances, TypeApplications, TypeFamilies, TypeSynonymInstances #-}

module ASCII.Superset ( AsciiSuperset (..), AsciiSupersetChar (..), AsciiSupersetString (..) ) where

import qualified Prelude
import Prelude ((.), (<=), Maybe (..), Bool (..), otherwise, all)
import Data.Functor (fmap)
import Data.Traversable (traverse)

import Data.Kind (Type)

import qualified Data.Char as Unicode
import qualified Data.String as Unicode

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified ASCII.Char as ASCII

class AsciiSuperset a
  where

    data AsciiRestricted a :: Type

    {-# MINIMAL validate, substitute, lift #-}

    restrictUnsafe :: a -> AsciiRestricted a
    restrictUnsafe = Maybe.fromJust . validate

    validate :: a -> Maybe (AsciiRestricted a)

    isValid :: a -> Bool
    isValid = Maybe.isJust . validate

    substitute :: a -> AsciiRestricted a

    lift :: AsciiRestricted a -> a

class AsciiSuperset a => AsciiSupersetChar a
  where
    {-# MINIMAL (fromChar | fromCharRestricted), (toChar | toCharUnsafe) #-}

    fromChar :: ASCII.Char -> a
    fromChar = lift . fromCharRestricted

    fromCharRestricted :: ASCII.Char -> AsciiRestricted a
    fromCharRestricted = restrictUnsafe . fromChar

    toChar :: AsciiRestricted a -> ASCII.Char
    toChar = toCharUnsafe . lift

    toCharUnsafe :: a -> ASCII.Char
    toCharUnsafe = toChar . Maybe.fromJust . validate

class AsciiSuperset a => AsciiSupersetString a
  where
    {-# MINIMAL (fromCharList | fromCharListRestricted), (toCharList | toCharListUnsafe) #-}

    fromCharList :: [ASCII.Char] -> a
    fromCharList = lift . fromCharListRestricted

    fromCharListRestricted :: [ASCII.Char] -> AsciiRestricted a
    fromCharListRestricted = restrictUnsafe . fromCharList

    toCharList :: AsciiRestricted a -> [ASCII.Char]
    toCharList = toCharListUnsafe . lift

    toCharListUnsafe :: a -> [ASCII.Char]
    toCharListUnsafe = toCharList . Maybe.fromJust . validate

instance AsciiSuperset Unicode.Char
  where

    newtype AsciiRestricted Unicode.Char = AsciiCharUnsafe { asciiChar :: Unicode.Char }

    restrictUnsafe = AsciiCharUnsafe

    lift = asciiChar

    isValid = (<= '\DEL')

    validate x   | isValid x = Just (restrictUnsafe x)
                 | otherwise = Nothing

    substitute x | isValid x = restrictUnsafe x
                 | otherwise = restrictUnsafe '\SUB'

instance AsciiSupersetChar Unicode.Char
  where

    toCharUnsafe = ASCII.fromIntUnsafe . Unicode.ord

    fromChar = Unicode.chr . ASCII.toInt

instance AsciiSuperset Unicode.String
  where

    newtype AsciiRestricted Unicode.String = AsciiStringUnsafe { asciiString :: Unicode.String }

    restrictUnsafe = AsciiStringUnsafe

    lift = asciiString

    isValid = all (isValid @Unicode.Char)

    validate x | isValid x = Just (restrictUnsafe x)
               | otherwise = Nothing

instance AsciiSupersetString Unicode.String
  where

    toCharListUnsafe = List.map (toCharUnsafe @Unicode.Char)

    fromCharList = List.map (lift . fromCharRestricted @Unicode.Char)
