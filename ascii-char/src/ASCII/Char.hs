{-# LANGUAGE DeriveLift, DeriveGeneric, NoImplicitPrelude, StandaloneDeriving #-}

module ASCII.Char where

import qualified Prelude
import qualified Language.Haskell.TH.Syntax as TH
import qualified GHC.Generics as G

-- | A character in the ASCII character set.
--
-- This type has 128 nullary constructors, listed in order according to each character's 7-bit numeric code. Its derived 'Enum' instance can therefore be used to convert a 'Char' to its ASCII code and vice versa; but since 'Enum.toEnum' is a partial function, we recommend instead using 'fromChar', which cannot produce runtime errors.

data Char =
      Null | StartOfHeading | StartOfText | EndOfText | EndOfTransmission | Enquiry | Acknowledgement | Bell | Backspace | HorizontalTab | LineFeed | VerticalTab | FormFeed | CarriageReturn | ShiftOut | ShiftIn | DataLinkEscape

    | DeviceControl1 | DeviceControl2 | DeviceControl3 | DeviceControl4

    | NegativeAcknowledgement | SynchronousIdle | EndOfTransmissionBlock | Cancel | EndOfMedium | Substitute | Escape

    | FileSeparator | GroupSeparator | RecordSeparator | UnitSeparator

    | Space | ExclamationMark | QuotationMark | NumberSign | DollarSign | PercentSign | Ampersand | Apostrophe | LeftParenthesis | RightParenthesis | Asterisk | PlusSign | Comma | HyphenMinus | FullStop | Slash

    | Digit0 | Digit1 | Digit2 | Digit3 | Digit4 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9

    | Colon | Semicolon | LessThanSign | EqualsSign | GreaterThanSign | QuestionMark | AtSign

    | CapitalLetterA | CapitalLetterB | CapitalLetterC | CapitalLetterD | CapitalLetterE | CapitalLetterF | CapitalLetterG | CapitalLetterH | CapitalLetterI | CapitalLetterJ | CapitalLetterK | CapitalLetterL | CapitalLetterM | CapitalLetterN | CapitalLetterO | CapitalLetterP | CapitalLetterQ | CapitalLetterR | CapitalLetterS | CapitalLetterT | CapitalLetterU | CapitalLetterV | CapitalLetterW | CapitalLetterX | CapitalLetterY | CapitalLetterZ

    | LeftSquareBracket | Backslash | RightSquareBracket | Caret | Underscore | GraveAccent

    | SmallLetterA | SmallLetterB | SmallLetterC | SmallLetterD | SmallLetterE | SmallLetterF | SmallLetterG | SmallLetterH | SmallLetterI | SmallLetterJ | SmallLetterK | SmallLetterL | SmallLetterM | SmallLetterN | SmallLetterO | SmallLetterP | SmallLetterQ | SmallLetterR | SmallLetterS | SmallLetterT | SmallLetterU | SmallLetterV | SmallLetterW | SmallLetterX | SmallLetterY | SmallLetterZ

    | LeftCurlyBracket | VerticalLine | RightCurlyBracket | Tilde | Delete

deriving instance Prelude.Eq Char
deriving instance Prelude.Ord Char
deriving instance Prelude.Enum Char
deriving instance Prelude.Bounded Char
deriving instance Prelude.Show Char

-- Requires the DeriveLift language extension.
deriving instance TH.Lift Char

-- Requires the DeriveGeneric language extension.
deriving instance G.Generic Char
