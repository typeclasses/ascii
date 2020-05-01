{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ASCII.Lift ( Lift (..) ) where

import ASCII.Char (Char)
import ASCII.Refinement (ASCII)
import ASCII.Superset (IsChar, IsString)

import qualified ASCII.Refinement
import qualified ASCII.Superset

{- $setup

>>> import ASCII.Char (Char (..))
>>> import Data.Text (Text)
>>> import Data.Word (Word8)

-}

{- | Converts from ASCII to any larger type.

>>> lift CapitalLetterA :: Word8
65

>>> lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text
"Hi!"

Due to the highly polymorphic nature of the 'lift' function, often it must used with an explicit type signature or type application to avoid any type ambiguity.

-}

class Lift ascii superset
  where
    lift :: ascii -> superset

instance Lift (ASCII.Refinement.ASCII superset) superset
  where
    lift = ASCII.Refinement.lift

instance IsChar superset => Lift Char superset
  where
    lift = ASCII.Superset.fromChar

instance IsString superset => Lift [Char] superset
  where
    lift = ASCII.Superset.fromCharList
