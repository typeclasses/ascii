module ASCII.TemplateHaskell
  (
    -- * Setup for examples
    -- $setup

    -- * Characters
      charExp, charPat

    -- * Character lists
    , charListExp, charListPat

  ) where

import qualified ASCII.Char as ASCII

import qualified Generics.Deriving.ConNames as G

import Language.Haskell.TH.Syntax ( Q, Exp, Pat, lift )
import qualified Language.Haskell.TH.Syntax as TH

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Fail (fail)
import Control.Monad ((>>=), return)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.List ((++))
import Data.Traversable (traverse)
import Text.Show (show)

import qualified Data.Maybe as Maybe

{- $setup

>>> :set -XTemplateHaskell
>>> import ASCII.Char
>>> import ASCII.Superset
>>> import ASCII.TemplateHaskell

-}

{- |

>>> $(toCharOrFail 'F' >>= charExp)
CapitalLetterF

>>> $(toCharOrFail '\DEL' >>= charExp)
Delete

-}

charExp :: ASCII.Char -> Q Exp
charExp = lift

{- |

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

{- |

>>> $(charListExp [CapitalLetterH, SmallLetterI])
[CapitalLetterH,SmallLetterI]

-}

charListExp :: [ASCII.Char] -> Q Exp
charListExp = lift

{- |

>>> :{
>>> case [CapitalLetterH, SmallLetterI] of
>>>     $(charListPat [CapitalLetterH, SmallLetterA]) -> 1
>>>     $(charListPat [CapitalLetterH, SmallLetterI]) -> 2
>>>     _                                             -> 3
>>> :}
2

-}

charListPat :: [ASCII.Char] -> Q Pat
charListPat = fmap TH.ListP . traverse charPat
