{-# OPTIONS_GHC -Wall #-}

module ASCII where

import Prelude (Bool, Eq, Enum, Bounded, Show, Ord (..), (&&))
import Data.Function ((.))

import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Integral (..), Int, fromIntegral)
import qualified Prelude as May (Maybe (..))
import qualified Data.Word as Word

data Char =
      Null
    | StartOfHeading
    | StartOfText
    | EndOfText
    | EndOfTransmission
    | Enquiry
    | Acknowledgement
    | Bell
    | Backspace
    | HorizontalTab
    | LineFeed
    | VerticalTab
    | FormFeed
    | CarriageReturn
    | ShiftOut
    | ShiftIn
    | DataLinkEscape
    | DeviceControl1
    | DeviceControl2
    | DeviceControl3
    | DeviceControl4
    | NegativeAcknowledgement
    | SynchronousIdle
    | EndOfTransmissionBlock
    | Cancel
    | EndOfMedium
    | Substitute
    | Escape
    | FileSeparator
    | GroupSeparator
    | RecordSeparator
    | UnitSeparator
    | Space
    | ExclamationMark
    | QuotationMark
    | NumberSign
    | DollarSign
    | PercentSign
    | Ampersand
    | Apostrophe
    | LeftParenthesis
    | RightParenthesis
    | Asterisk
    | PlusSign
    | Comma
    | HyphenMinus
    | FullStop
    | Slash
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Colon
    | Semicolon
    | LessThanSign
    | EqualsSign
    | GreaterThanSign
    | QuestionMark
    | AtSign
    | CapitalLetterA
    | CapitalLetterB
    | CapitalLetterC
    | CapitalLetterD
    | CapitalLetterE
    | CapitalLetterF
    | CapitalLetterG
    | CapitalLetterH
    | CapitalLetterI
    | CapitalLetterJ
    | CapitalLetterK
    | CapitalLetterL
    | CapitalLetterM
    | CapitalLetterN
    | CapitalLetterO
    | CapitalLetterP
    | CapitalLetterQ
    | CapitalLetterR
    | CapitalLetterS
    | CapitalLetterT
    | CapitalLetterU
    | CapitalLetterV
    | CapitalLetterW
    | CapitalLetterX
    | CapitalLetterY
    | CapitalLetterZ
    | LeftSquareBracket
    | Backslash
    | RightSquareBracket
    | Caret
    | Underscore
    | GraveAccent
    | SmallLetterA
    | SmallLetterB
    | SmallLetterC
    | SmallLetterD
    | SmallLetterE
    | SmallLetterF
    | SmallLetterG
    | SmallLetterH
    | SmallLetterI
    | SmallLetterJ
    | SmallLetterK
    | SmallLetterL
    | SmallLetterM
    | SmallLetterN
    | SmallLetterO
    | SmallLetterP
    | SmallLetterQ
    | SmallLetterR
    | SmallLetterS
    | SmallLetterT
    | SmallLetterU
    | SmallLetterV
    | SmallLetterW
    | SmallLetterX
    | SmallLetterY
    | SmallLetterZ
    | LeftCurlyBracket
    | VerticalLine
    | RightCurlyBracket
    | Tilde
    | Delete
  deriving (Eq, Ord, Enum, Bounded, Show)

intInRange :: Num.Int -> Bool
intInRange x = (x >= 0) && (x <= 127)

intChar :: Num.Int -> Char
intChar x = if intInRange x then Enum.toEnum x else Substitute

word8Char :: Word.Word8 -> Char
word8Char = Enum.toEnum . Num.fromIntegral

integralChar :: Num.Integral int => int -> Char
integralChar = intChar . Num.fromIntegral

intCharMaybe :: Num.Int -> May.Maybe Char
intCharMaybe x = if intInRange x then May.Just (Enum.toEnum x) else May.Nothing

integralCharMaybe :: Num.Integral int => int -> May.Maybe Char
integralCharMaybe = intCharMaybe . Num.fromIntegral

charInt :: Char -> Num.Int
charInt = Enum.fromEnum

charWord8 :: Char -> Word.Word8
charWord8 = Num.fromIntegral . Enum.fromEnum

charIntegral :: Num.Integral int => Char -> int
charIntegral = Num.fromIntegral . charInt
