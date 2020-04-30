module ASCII.TemplateHaskell
  (
    -- * Setup for examples
    -- $setup

    -- * Characters
    charExp, charPat

  ) where

import qualified ASCII.Char as ASCII

import qualified Generics.Deriving.ConNames as G

import Language.Haskell.TH.Syntax ( Q, Exp, Pat, lift )
import qualified Language.Haskell.TH.Syntax as TH

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Fail
import Control.Monad
import Data.List ((++))
import Text.Show (show)

import qualified Data.Maybe as Maybe

{- $setup

>>> :set -XTemplateHaskell
>>> import ASCII.Char
>>> import ASCII.Superset
>>> import ASCII.TemplateHaskell

-}

{- |

==== Examples

>>> $(toCharOrFail 'F' >>= charExp)
CapitalLetterF

>>> $(toCharOrFail '\DEL' >>= charExp)
Delete

-}

charExp :: ASCII.Char -> Q Exp
charExp = lift

{- |

==== Examples

>>> :{
>>> case SmallLetterS of
>>>     $(toCharOrFail 'r' >>= charPat) -> 1
>>>     $(toCharOrFail 's' >>= charPat) -> 2
>>>     _                               -> 3
>>> :}
2

This is the same as:

>>> :{
>>> case SmallLetterS of
>>>     SmallLetterR -> 1
>>>     SmallLetterS -> 2
>>>     _            -> 3
>>> :}
2

-}

charPat :: ASCII.Char -> Q Pat
charPat c = TH.ConP <$> lookupConName <*> return []
  where
    conName = "ASCII.Char." ++ G.conNameOf c
    lookupConName = TH.lookupValueName conName >>= Maybe.maybe lookupFailed return
    lookupFailed = fail ("lookupValueName " ++ show conName ++ " failed.")
