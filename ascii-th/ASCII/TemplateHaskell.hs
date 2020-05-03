module ASCII.TemplateHaskell (

    {- * Characters          -} charExp,     charPat,
    {- * Character lists     -} charListExp, charListPat,
    {- * Character supersets -} isCharExp,   isCharPat,
    {- * String supersets    -} isStringExp, isStringPat

  ) where

import qualified ASCII.Char       as  ASCII
import qualified ASCII.Superset

import Data.Data                  ( Data )
import Data.Maybe                 ( Maybe (..) )
import Language.Haskell.TH.Syntax ( Q, Exp, Pat, dataToExpQ, dataToPatQ )

{- $setup

>>> :set -XNoViewPatterns
>>> import ASCII.Char (Char (..))
>>> import ASCII.Refinement (ASCII)
>>> import ASCII.Superset (toCharOrFail)
>>> import ASCII.TemplateHaskell (charExp)
>>> import Control.Monad ((>>=))
>>> import Data.Word (Word8)

-}

exp :: Data a => a -> Q Exp
exp = dataToExpQ (\_ -> Nothing)

pat :: Data a => a -> Q Pat
pat = dataToPatQ (\_ -> Nothing)

{- |

>>> $(toCharOrFail 'F' >>= charExp)
CapitalLetterF

>>> $(toCharOrFail '\DEL' >>= charExp)
Delete

-}

charExp :: ASCII.Char -> Q Exp
charExp = exp

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
charPat = pat

{- |

>>> $(charListExp [CapitalLetterH, SmallLetterI])
[CapitalLetterH,SmallLetterI]

-}

charListExp :: [ASCII.Char] -> Q Exp
charListExp = exp

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
charListPat = pat

{- |

>>> $(isCharExp CapitalLetterA) :: ASCII.Char
CapitalLetterA

>>> $(isCharExp CapitalLetterA) :: Word8
65

>>> $(isCharExp CapitalLetterA) :: ASCII Word8
asciiUnsafe 65

-}

isCharExp :: ASCII.Char -> Q Exp
isCharExp x = [| ASCII.Superset.fromChar $(charExp x) |]

{- |

>>> :set -XViewPatterns

>>> :{
>>> case (66 :: Word8) of
>>>     $(isCharPat CapitalLetterA) -> 1
>>>     $(isCharPat CapitalLetterB) -> 2
>>>     _                           -> 3
>>> :}
2

-}

isCharPat :: ASCII.Char -> Q Pat
isCharPat x = [p| (ASCII.Superset.toCharMaybe -> Just $(charPat x)) |]

isStringExp :: [ASCII.Char] -> Q Exp
isStringExp xs = [| ASCII.Superset.fromCharList $(charListExp xs) |]

isStringPat :: [ASCII.Char] -> Q Pat
isStringPat xs = [p| (ASCII.Superset.toCharListMaybe -> Just $(charListPat xs)) |]
