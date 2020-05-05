{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ASCII.Lift ( Lift (..) ) where

import ASCII.Char       ( Char )
import ASCII.Refinement ( ASCII )
import ASCII.Superset   ( CharSuperset, StringSuperset )

import qualified ASCII.Refinement as R
import qualified ASCII.Superset as S

{- $setup

>>> import ASCII.Char (Char (..))
>>> import Data.Text (Text)
>>> import Data.Word (Word8)

-}

class Lift ascii superset
  where

    {- | Converts from ASCII to any larger type.

    >>> lift CapitalLetterA :: Word8
    65

    >>> lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text
    "Hi!"

    Due to the highly polymorphic nature of the 'lift' function, often it must used with an explicit type signature or type application to avoid any type ambiguity.

    -}

    lift :: ascii -> superset

-- | A value from an ASCII superset that has been refined by the 'ASCII' type constructor may be lifted back into the superset by unwrapping it from the 'ASCII' type.

instance Lift (ASCII superset) superset where lift = R.lift

-- | An ASCII 'Char' may be 'lift'ed into any larger character set (a 'CharSuperset').

instance CharSuperset superset => Lift Char superset where lift = S.fromChar

-- | An ASCII 'Char' list may be 'lift'ed into a string of any larger character set (a 'StringSuperset').

instance StringSuperset superset => Lift [Char] superset where lift = S.fromCharList
