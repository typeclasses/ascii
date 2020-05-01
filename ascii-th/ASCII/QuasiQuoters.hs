module ASCII.QuasiQuoters
  (
    -- * Setup for examples
    -- $setup

    -- * Quasi-quoters
    char
  , charList

  ) where

import ASCII.Char
import ASCII.Superset
import ASCII.TemplateHaskell

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Control.Monad ((>=>), return)
import Control.Monad.Fail (MonadFail, fail)

import Data.Maybe (Maybe (..))

import qualified Data.Char as Unicode
import qualified Data.String as Unicode

{- $setup

>>> :set -XQuasiQuotes
>>> :set -fno-warn-overlapping-patterns
>>> import ASCII.Char
>>> import ASCII.QuasiQuoters

-}

{- | Produces an expression or a pattern corresponding to an ASCII 'Char'.

The quasi-quoted string must consist of a single character that is within the ASCII character set.

>>> [char|e|]
SmallLetterE

>>> case Tilde of [char|@|] -> 1; [char|~|] -> 2; _ -> 3
2

-}

char :: QuasiQuoter
char = expPatQQ requireOneAscii charExp charPat

{- | Produces an expression or a pattern corresponding to an ASCII 'Char' list.

The quasi-quoted string must consist only of characters are within the ASCII character set.

>>> [charList|Hello!|]
[CapitalLetterH,SmallLetterE,SmallLetterL,SmallLetterL,SmallLetterO,ExclamationMark]

>>> case [CapitalLetterH, SmallLetterI] of [charList|Bye|] -> 1; [charList|Hi|] -> 2; _ -> 3
2

-}

charList :: QuasiQuoter
charList = expPatQQ requireAsciiList charListExp charListPat

requireOneAscii :: Unicode.String -> Q Char
requireOneAscii = requireOne >=> requireAscii

oneMaybe :: [a] -> Maybe a
oneMaybe xs = case xs of [x] -> Just x; _ -> Nothing

requireOne :: Unicode.String -> Q Unicode.Char
requireOne = oneMaybe || "Must be exactly one character."

requireAscii :: Unicode.Char -> Q Char
requireAscii = toCharMaybe || "Must be an ASCII character."

requireAsciiList :: Unicode.String -> Q [Char]
requireAsciiList = toCharListMaybe || "Must be only ASCII characters."

(||) :: (a -> Maybe b) -> Unicode.String -> a -> Q b
f || msg = \a -> case f a of Just b -> return b; Nothing -> fail msg

expPatQQ :: (Unicode.String -> Q a) -> (a -> Q Exp) -> (a -> Q Pat) -> QuasiQuoter
expPatQQ f a b =
  QuasiQuoter
    { quoteExp = f >=> a
    , quotePat = f >=> b
    , quoteType = notType
    , quoteDec = notDec
    }

notType :: MonadFail m => a -> m b
notType _ = fail "Cannot be used in a type context."

notDec :: MonadFail m => a -> m b
notDec _ = fail "Cannot be used in a declaration context."
