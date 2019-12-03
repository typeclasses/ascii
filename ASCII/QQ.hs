{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

{- |

Quasi-quoter for writing ASCII 'ASCII.String' literals.

== Recommended import style

> import ASCII.QQ

-}

module ASCII.QQ (ascii) where

import qualified ASCII (pack, unpack, fromUnicodeMaybe)
import qualified Control.Monad.Fail (fail)
import Data.Maybe (Maybe (..))

-- Template Haskell
import qualified Language.Haskell.TH.Quote as QQ (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH (Lift (lift))
import Language.Haskell.TH.Syntax (Exp, Q)

-- The two String types
import qualified ASCII (String)
import qualified Prelude as Unicode (String)

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
>     • The ascii quasi-quoter cannot be used with non-ASCII characters.
>     • In the quasi-quotation: [ascii|helloλ|]

-}

ascii :: QQ.QuasiQuoter
ascii =
  QQ.QuasiQuoter
    { QQ.quoteExp  = asciiQuoteExp
    , QQ.quotePat  = wrongContext
    , QQ.quoteType = wrongContext
    , QQ.quoteDec  = wrongContext
    }

wrongContext :: Unicode.String -> Q a
wrongContext _ = failQ "The ascii quasi-quoter may only be used in an expression context."

failQ :: Unicode.String -> Q a
failQ = Control.Monad.Fail.fail

asciiQuoteExp :: Unicode.String -> Q Exp
asciiQuoteExp str =
    case ASCII.fromUnicodeMaybe str of
        Nothing -> failQ "The ascii quasi-quoter cannot be used with non-ASCII characters."
        Just x -> asciiExp x

asciiExp :: ASCII.String -> Q Exp
asciiExp x = [e|ASCII.pack $(TH.lift (ASCII.unpack x))|]
