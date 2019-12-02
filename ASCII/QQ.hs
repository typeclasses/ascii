{-# LANGUAGE TemplateHaskell #-}

module ASCII.QQ (ascii) where

import qualified ASCII

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Lift (lift), Q)

import qualified Control.Monad.Fail as Fail

ascii :: QuasiQuoter
ascii =
  QuasiQuoter
    { quoteExp  = stringAsciiExp
    , quotePat  = \_ -> fail "ascii cannot be used in a pattern context"
    , quoteType = \_ -> fail "ascii cannot be used in a type context"
    , quoteDec  = \_ -> fail "ascii cannot be used in a declaration context"
    }

stringAsciiExp :: String -> Q Exp
stringAsciiExp str =
  case ASCII.fromUnicodeMaybe str of
    Nothing -> Fail.fail "ascii cannot be used with non-ASCII characters"
    Just x -> [e|ASCII.pack $(lift (ASCII.unpack x))|]
