module ASCII.Predicates
  (
    {- * Group predicates -} isControl, isPrint,
    {- * Case predicates -} isLower, isUpper,
    {- * Letter predicates -} isLetter, isAlpha,
    {- * Number predicates -} isDigit, isOctDigit, isHexDigit, isNumber,
    {- * Miscellaneous predicates -} isSpace, isAlphaNum, isMark, isPunctuation, isSymbol, isSeparator, isVisible,
    {- * Notes -} {- $notes -}
  )
  where

import ASCII.Char (Char (..))
import Data.Bool (Bool (..), otherwise)
import Data.Eq ((==))
import Data.Function ((.))
import Data.Ord ((<), (<=), (>=))

import qualified ASCII.Char as Char
import qualified Data.Bool as Bool
import qualified Data.List as List

{- | Returns True for control characters.

This function is analogous to 'Data.Char.isControl' in the "Data.Char" module.

-}

isControl :: Char -> Bool
isControl x =
    case x of
        _ | (x < Char.Space) -> True
        Char.Delete          -> True
        _                    -> False

{- | Returns True for printable characters.

This function is analogous to 'Data.Char.isPrint' in the "Data.Char" module.

-}

isPrint :: Char -> Bool
isPrint = Bool.not . isControl

{- | Returns True for visible characters.

This includes all print characters except 'Char.Space'.

-}

isVisible :: Char -> Bool
isVisible x =
    case x of
        _ | x <= Char.Space -> False
        Char.Delete         -> False
        _                   -> True

{- | Returns True for lower-case letters, from 'SmallLetterA' to 'SmallLetterZ'.

This function is analogous to 'Data.Char.isLower' in the "Data.Char" module.

-}

isLower :: Char -> Bool
isLower x = (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterZ)

{- | Returns True for upper-case letters, from 'CapitalLetterA' to 'CapitalLetterZ'.

This function is analogous to 'Data.Char.isUpper' in the "Data.Char" module.

-}

isUpper :: Char -> Bool
isUpper x = (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterZ)

{- | Returns True for letters:

- 'SmallLetterA' to 'SmallLetterZ'
- 'CapitalLetterA' to 'CapitalLetterZ'

This function is analogous to 'Data.Char.isLetter' in the "Data.Char" module.

-}

isLetter :: Char -> Bool
isLetter x = (Bool.||) (isLower x) (isUpper x)

{- | Synonym for 'isLetter'.

This function is analogous to 'Data.Char.isAlpha' in the "Data.Char" module.

-}

isAlpha :: Char -> Bool
isAlpha = isLetter

{- | Returns True for the characters from 'Digit0' to 'Digit9'.

This function is analogous to 'Data.Char.isDigit' in the "Data.Char" module.

-}

isDigit :: Char -> Bool
isDigit x = (Bool.&&) (x >= Digit0) (x <= Digit9)

{- | Returns True for the characters from 'Digit0' to 'Digit7'.

This function is analogous to 'Data.Char.isOctDigit' in the "Data.Char" module.

-}

isOctDigit :: Char -> Bool
isOctDigit x = (Bool.&&) (x >= Digit0) (x <= Digit7)

{- | Returns True for characters in any of the following ranges:

- 'Digit0' to 'Digit9'
- 'CapitalLetterA' to 'CapitalLetterF'
- 'SmallLetterA' to 'SmallLetterF'

This function is analogous to 'Data.Char.isHexDigit' in the "Data.Char" module.

-}

isHexDigit :: Char -> Bool
isHexDigit x | isDigit x = True
             | (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterF) = True
             | (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterF) = True
             | otherwise = False

{- | Synonym for 'isDigit'.

In the "Data.Char" module, 'Data.Char.isDigit' selects only the ASCII digits 0 through 9, and 'Data.Char.isNumber' selects a wider set of characters because the full Unicode character set contains more numeric characters than just the ASCII digits. In this module, these two functions are redundant, but we include this synonym for compatibility with "Data.Char".

-}

isNumber :: Char -> Bool
isNumber = isDigit

{- | Returns True for the following characters:

- 'Space'
- 'HorizontalTab'
- 'LineFeed'
- 'VerticalTab'
- 'FormFeed'
- 'CarriageReturn'

This function is analogous to 'Data.Char.isSpace' in the "Data.Char" module.

-}

isSpace :: Char -> Bool
isSpace Space = True
isSpace x = (Bool.&&) (x >= HorizontalTab) (x <= CarriageReturn)

-- | This function is analogous to 'Data.Char.isAlphaNum' in the "Data.Char" module.

isAlphaNum :: Char -> Bool
isAlphaNum x = (Bool.||) (isAlpha x) (isDigit x)

-- | Selects mark characters, for example accents and the like, which combine with preceding characters. This always returns False because ASCII does not include any mark characters. This function is included only for compatibility with 'Data.Char.isMark' in the "Data.Char" module.

isMark :: Char -> Bool
isMark _ = False

{- | Returns True for the following characters:

- 'ExclamationMark'
- 'QuotationMark'
- 'NumberSign'
- 'PercentSign'
- 'Ampersand'
- 'Apostrophe'
- 'LeftParenthesis'
- 'RightParenthesis'
- 'Asterisk'
- 'Comma'
- 'HyphenMinus'
- 'FullStop'
- 'Slash'
- 'Colon'
- 'Semicolon'
- 'QuestionMark'
- 'AtSign'
- 'LeftSquareBracket'
- 'Backslash'
- 'RightSquareBracket'
- 'Underscore'
- 'LeftCurlyBracket'
- 'RightCurlyBracket'

This function is analogous to 'Data.Char.isPunctuation' in the "Data.Char" module.

-}

isPunctuation :: Char -> Bool
isPunctuation = (`List.elem` [ExclamationMark, QuotationMark, NumberSign, PercentSign, Ampersand, Apostrophe, LeftParenthesis, RightParenthesis, Asterisk, Comma, HyphenMinus, FullStop, Slash, Colon, Semicolon, QuestionMark, AtSign, LeftSquareBracket, Backslash, RightSquareBracket, Underscore, LeftCurlyBracket, RightCurlyBracket])

{- | Returns True for the following characters:

- 'DollarSign'
- 'PlusSign'
- 'LessThanSign'
- 'EqualsSign'
- 'GreaterThanSign'
- 'Caret'
- 'GraveAccent'
- 'VerticalLine'
- 'Tilde'

This function is analogous to 'Data.Char.isSymbol' in the "Data.Char" module.

-}

isSymbol :: Char -> Bool
isSymbol = (`List.elem` [DollarSign, PlusSign, LessThanSign, EqualsSign, GreaterThanSign, Caret, GraveAccent, VerticalLine, Tilde])

{- | Returns True if the character is 'Space'.

This function is analogous to 'Data.Char.isSeparator' in the "Data.Char" module.

-}

isSeparator :: Char -> Bool
isSeparator = (== Space)

{- $notes

This module defines drop-in replacements for closely related definitions of the same name in the "Data.Char" module.

>>> import qualified Data.Char
>>> import qualified Data.List
>>> convert = Data.Char.chr . ASCII.Char.toInt
>>> eq f g = Data.List.all (\x -> f x == g (convert x)) ASCII.Char.allCharacters

>>> eq isControl Data.Char.isControl
True

>>> eq isSpace Data.Char.isSpace
True

>>> eq isLower Data.Char.isLower
True

>>> eq isUpper Data.Char.isUpper
True

>>> eq isAlpha Data.Char.isAlpha
True

>>> eq isAlphaNum Data.Char.isAlphaNum
True

>>> eq isPrint Data.Char.isPrint
True

>>> eq isDigit Data.Char.isDigit
True

>>> eq isOctDigit Data.Char.isOctDigit
True

>>> eq isHexDigit Data.Char.isHexDigit
True

>>> eq isLetter Data.Char.isLetter
True

>>> eq isMark Data.Char.isMark
True

>>> eq isNumber Data.Char.isNumber
True

>>> eq isPunctuation Data.Char.isPunctuation
True

>>> eq isSymbol Data.Char.isSymbol
True

>>> eq isSeparator Data.Char.isSeparator
True

-}
