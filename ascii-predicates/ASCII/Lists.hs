module ASCII.Lists
  (
    -- * Lists
    -- ** Every character
      all
    -- ** Group-related
    , printableCharacters, controlCodes
    -- ** Letter-related
    , letters, capitalLetters, smallLetters
    -- ** Number-related
    , digits, octDigits, hexDigits, numbers

    -- * Notes
    -- $notes

  ) where

import ASCII.Char (Char (..))

import qualified Data.List as List
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Enum (Enum (..))

{- $setup

>>> import Data.Eq ((==))
>>> import Data.List (sort)

-}

-- | All 128 ASCII characters, listed in order from 'Null' to 'Delete'.

all :: [Char]
all = Enum.enumFromTo Bounded.minBound Bounded.maxBound

-- | Characters in the 'Printable' group: 'Space', 'ExclamationMark', 'QuotationMark', 'NumberSign', 'DollarSign', 'PercentSign', 'Ampersand', 'Apostrophe', 'LeftParenthesis', 'RightParenthesis', 'Asterisk', 'PlusSign', 'Comma', 'HyphenMinus', 'FullStop', 'Slash', 'Digit0', 'Digit1', 'Digit2', 'Digit3', 'Digit4', 'Digit5', 'Digit6', 'Digit7', 'Digit8', 'Digit9', 'Colon', 'Semicolon', 'LessThanSign', 'EqualsSign', 'GreaterThanSign', 'QuestionMark', 'AtSign', 'CapitalLetterA', 'CapitalLetterB', 'CapitalLetterC', 'CapitalLetterD', 'CapitalLetterE', 'CapitalLetterF', 'CapitalLetterG', 'CapitalLetterH', 'CapitalLetterI', 'CapitalLetterJ', 'CapitalLetterK', 'CapitalLetterL', 'CapitalLetterM', 'CapitalLetterN', 'CapitalLetterO', 'CapitalLetterP', 'CapitalLetterQ', 'CapitalLetterR', 'CapitalLetterS', 'CapitalLetterT', 'CapitalLetterU', 'CapitalLetterV', 'CapitalLetterW', 'CapitalLetterX', 'CapitalLetterY', 'CapitalLetterZ', 'LeftSquareBracket', 'Backslash', 'RightSquareBracket', 'Caret', 'Underscore', 'GraveAccent', 'SmallLetterA', 'SmallLetterB', 'SmallLetterC', 'SmallLetterD', 'SmallLetterE', 'SmallLetterF', 'SmallLetterG', 'SmallLetterH', 'SmallLetterI', 'SmallLetterJ', 'SmallLetterK', 'SmallLetterL', 'SmallLetterM', 'SmallLetterN', 'SmallLetterO', 'SmallLetterP', 'SmallLetterQ', 'SmallLetterR', 'SmallLetterS', 'SmallLetterT', 'SmallLetterU', 'SmallLetterV', 'SmallLetterW', 'SmallLetterX', 'SmallLetterY', 'SmallLetterZ', 'LeftCurlyBracket', 'VerticalLine', 'RightCurlyBracket', 'Tilde'.

printableCharacters :: [Char]
printableCharacters = Enum.enumFromTo Space Tilde

-- | Characters in the 'Control' group: 'Null', 'StartOfHeading', 'StartOfText', 'EndOfText', 'EndOfTransmission', 'Enquiry', 'Acknowledgement', 'Bell', 'Backspace', 'HorizontalTab', 'LineFeed', 'VerticalTab', 'FormFeed', 'CarriageReturn', 'ShiftOut', 'ShiftIn', 'DataLinkEscape', 'DeviceControl1', 'DeviceControl2', 'DeviceControl3', 'DeviceControl4', 'NegativeAcknowledgement', 'SynchronousIdle', 'EndOfTransmissionBlock', 'Cancel', 'EndOfMedium', 'Substitute', 'Escape', 'FileSeparator', 'GroupSeparator', 'RecordSeparator', 'UnitSeparator', 'Delete'.

controlCodes :: [Char]
controlCodes = (List.++) (Enum.enumFromTo Null UnitSeparator) [Delete]

-- | Letters: 'CapitalLetterA', 'CapitalLetterB', 'CapitalLetterC', 'CapitalLetterD', 'CapitalLetterE', 'CapitalLetterF', 'CapitalLetterG', 'CapitalLetterH', 'CapitalLetterI', 'CapitalLetterJ', 'CapitalLetterK', 'CapitalLetterL', 'CapitalLetterM', 'CapitalLetterN', 'CapitalLetterO', 'CapitalLetterP', 'CapitalLetterQ', 'CapitalLetterR', 'CapitalLetterS', 'CapitalLetterT', 'CapitalLetterU', 'CapitalLetterV', 'CapitalLetterW', 'CapitalLetterX', 'CapitalLetterY', 'CapitalLetterZ', 'SmallLetterA', 'SmallLetterB', 'SmallLetterC', 'SmallLetterD', 'SmallLetterE', 'SmallLetterF', 'SmallLetterG', 'SmallLetterH', 'SmallLetterI', 'SmallLetterJ', 'SmallLetterK', 'SmallLetterL', 'SmallLetterM', 'SmallLetterN', 'SmallLetterO', 'SmallLetterP', 'SmallLetterQ', 'SmallLetterR', 'SmallLetterS', 'SmallLetterT', 'SmallLetterU', 'SmallLetterV', 'SmallLetterW', 'SmallLetterX', 'SmallLetterY', 'SmallLetterZ'.

letters :: [Char]
letters = (List.++) capitalLetters smallLetters

-- | Capital letters: 'CapitalLetterA', 'CapitalLetterB', 'CapitalLetterC', 'CapitalLetterD', 'CapitalLetterE', 'CapitalLetterF', 'CapitalLetterG', 'CapitalLetterH', 'CapitalLetterI', 'CapitalLetterJ', 'CapitalLetterK', 'CapitalLetterL', 'CapitalLetterM', 'CapitalLetterN', 'CapitalLetterO', 'CapitalLetterP', 'CapitalLetterQ', 'CapitalLetterR', 'CapitalLetterS', 'CapitalLetterT', 'CapitalLetterU', 'CapitalLetterV', 'CapitalLetterW', 'CapitalLetterX', 'CapitalLetterY', 'CapitalLetterZ'.

capitalLetters :: [Char]
capitalLetters = Enum.enumFromTo CapitalLetterA CapitalLetterZ

-- | Small letters: 'SmallLetterA', 'SmallLetterB', 'SmallLetterC', 'SmallLetterD', 'SmallLetterE', 'SmallLetterF', 'SmallLetterG', 'SmallLetterH', 'SmallLetterI', 'SmallLetterJ', 'SmallLetterK', 'SmallLetterL', 'SmallLetterM', 'SmallLetterN', 'SmallLetterO', 'SmallLetterP', 'SmallLetterQ', 'SmallLetterR', 'SmallLetterS', 'SmallLetterT', 'SmallLetterU', 'SmallLetterV', 'SmallLetterW', 'SmallLetterX', 'SmallLetterY', 'SmallLetterZ'.

smallLetters :: [Char]
smallLetters = Enum.enumFromTo SmallLetterA SmallLetterZ

-- | Digits: 'Digit0', 'Digit1', 'Digit2', 'Digit3', 'Digit4', 'Digit5', 'Digit6', 'Digit7', 'Digit8', 'Digit9'.

digits :: [Char]
digits = Enum.enumFromTo Digit0 Digit9

-- | Octal digits: 'Digit0', 'Digit1', 'Digit2', 'Digit3', 'Digit4', 'Digit5', 'Digit6', 'Digit7'.

octDigits :: [Char]
octDigits = Enum.enumFromTo Digit0 Digit7

-- | Hexidecimal digits: 'Digit0', 'Digit1', 'Digit2', 'Digit3', 'Digit4', 'Digit5', 'Digit6', 'Digit7', 'Digit8', 'Digit9', 'CapitalLetterA', 'CapitalLetterB', 'CapitalLetterC', 'CapitalLetterD', 'CapitalLetterE', 'CapitalLetterF', 'SmallLetterA', 'SmallLetterB', 'SmallLetterC', 'SmallLetterD', 'SmallLetterE', 'SmallLetterF'.

hexDigits :: [Char]
hexDigits =
  List.concat
    [ digits
    , Enum.enumFromTo CapitalLetterA CapitalLetterF
    , Enum.enumFromTo SmallLetterA SmallLetterF
    ]

-- | Synonym for 'digits'.

numbers :: [Char]
numbers = digits

{- $notes

Each list is sorted in ascending order.

>>> lists = [all, printableCharacters, controlCodes, letters, capitalLetters, smallLetters, digits, octDigits, hexDigits, numbers]

>>> Prelude.all (\xs -> sort xs == xs) lists
True

-}
