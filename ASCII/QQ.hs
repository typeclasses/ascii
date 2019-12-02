{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TemplateHaskell #-}

module ASCII.QQ (ascii) where

import qualified ASCII

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Lift (lift), Q)

import qualified Control.Monad.Fail as Fail

{- | Produces an expression representing a 'ASCII.String' value corresponding to the quasiquoted string.

> >>> :set -XQuasiQuotes
>
> >>> import qualified ASCII
>
> >>> import ASCII.QQ
>
> >>> ASCII.unpack [ascii|Hi!|]
> [CapitalLetterH,SmallLetterI,ExclamationMark]

The expression fails to compile if the quasiquoted string contains characters that cannot be represented in the ASCII character set.

> >>> [ascii|helloλ|]
>
> interactive>:4:8: error:
>     • ascii cannot be used with non-ASCII characters
>     • In the quasi-quotation: [ascii|helloλ|]

-}

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
