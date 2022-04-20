{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ASCII.Lift ( Lift (..) ) where

import ASCII.Char (Char)
import ASCII.Refinement (ASCII)
import ASCII.Superset (CharSuperset, StringSuperset)

import qualified ASCII.Refinement as R
import qualified ASCII.Superset as S

import qualified Prelude

-- | Embedding of one character set within another
--
-- The @subset@ and @superset@ types may be characters or strings in ASCII, some subset of ASCII, or some superset of ASCII.
--
class Lift subset superset
  where

    {- | Converts from a smaller to a larger type.

    >>> lift CapitalLetterA :: Word8
    65

    >>> lift [CapitalLetterH,SmallLetterI,ExclamationMark] :: Text
    "Hi!"

    Due to the highly polymorphic nature of the 'lift' function, often it must used with an explicit type signature or type application to avoid any type ambiguity.

    -}

    lift :: subset -> superset

-- | A value from an ASCII superset that has been refined by the 'ASCII' type constructor may be lifted back into the superset by unwrapping it from the 'ASCII' type.

instance Lift (ASCII superset) superset where lift = R.lift

-- | An ASCII 'Char' may be 'lift'ed into any larger character set (a 'CharSuperset'); for example, 'lift' can convert an ASCII character into a value of the standard 'Prelude.Char' type in "Prelude".

instance CharSuperset superset => Lift Char superset where lift = S.fromChar

-- | An ASCII 'Char' list may be 'lift'ed into a string of any larger character set (a 'StringSuperset'); for example, 'lift' can convert a list of ASCII characters into a value of the standard 'Prelude.String' type in "Prelude".

instance StringSuperset superset => Lift [Char] superset where lift = S.fromCharList
