module Main (main) where

import ASCII.Caseless
import ASCII.Case (Case (..))

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Bool (Bool (..), (&&), (||))
import Data.Eq (Eq ((==)))
import Data.Foldable (all)
import Data.Function (($), (.), (&), flip)
import Data.Functor (Functor (..))
import Data.List (intercalate, length, map, null)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import System.Exit (die)
import System.IO (IO, putStrLn)
import Text.Show (show)

import qualified ASCII.Case as Case
import qualified ASCII.Char as CaseSensitive
import qualified ASCII.Caseless as Caseless

main :: IO ()
main = dieIfFailures $ do
    test 1 $ length allCharacters == 102
    test 2 testDisregardCase
    test 3 testToUpper
    test 4 testToLower
    test 5 testAssumeCaseUnsafe

testAssumeCaseUnsafe :: Bool
testAssumeCaseUnsafe =
    [UpperCase, LowerCase] & flip all $ \letterCase ->
        CaseSensitive.allCharacters & flip all $ \x ->
            Case.isCase (oppositeCase letterCase) x  -- (assumeCaseUnsafe letterCase x) is not defined
            || Caseless.assumeCaseUnsafe letterCase x == Caseless.disregardCase x

oppositeCase :: Case -> Case
oppositeCase UpperCase = LowerCase
oppositeCase LowerCase = UpperCase

testDisregardCase :: Bool
testDisregardCase =
    disregardCase CaseSensitive.Null == Caseless.Null &&
    disregardCase CaseSensitive.StartOfHeading == Caseless.StartOfHeading &&
    disregardCase CaseSensitive.StartOfText == Caseless.StartOfText &&
    disregardCase CaseSensitive.EndOfText == Caseless.EndOfText &&
    disregardCase CaseSensitive.EndOfTransmission == Caseless.EndOfTransmission &&
    disregardCase CaseSensitive.Enquiry == Caseless.Enquiry &&
    disregardCase CaseSensitive.Acknowledgement == Caseless.Acknowledgement &&
    disregardCase CaseSensitive.Bell == Caseless.Bell &&
    disregardCase CaseSensitive.Backspace == Caseless.Backspace &&
    disregardCase CaseSensitive.HorizontalTab == Caseless.HorizontalTab &&
    disregardCase CaseSensitive.LineFeed == Caseless.LineFeed &&
    disregardCase CaseSensitive.VerticalTab == Caseless.VerticalTab &&
    disregardCase CaseSensitive.FormFeed == Caseless.FormFeed &&
    disregardCase CaseSensitive.CarriageReturn == Caseless.CarriageReturn &&
    disregardCase CaseSensitive.ShiftOut == Caseless.ShiftOut &&
    disregardCase CaseSensitive.ShiftIn == Caseless.ShiftIn &&
    disregardCase CaseSensitive.DataLinkEscape == Caseless.DataLinkEscape &&
    disregardCase CaseSensitive.DeviceControl1 == Caseless.DeviceControl1 &&
    disregardCase CaseSensitive.DeviceControl2 == Caseless.DeviceControl2 &&
    disregardCase CaseSensitive.DeviceControl3 == Caseless.DeviceControl3 &&
    disregardCase CaseSensitive.DeviceControl4 == Caseless.DeviceControl4 &&
    disregardCase CaseSensitive.NegativeAcknowledgement == Caseless.NegativeAcknowledgement &&
    disregardCase CaseSensitive.SynchronousIdle == Caseless.SynchronousIdle &&
    disregardCase CaseSensitive.EndOfTransmissionBlock == Caseless.EndOfTransmissionBlock &&
    disregardCase CaseSensitive.Cancel == Caseless.Cancel &&
    disregardCase CaseSensitive.EndOfMedium == Caseless.EndOfMedium &&
    disregardCase CaseSensitive.Substitute == Caseless.Substitute &&
    disregardCase CaseSensitive.Escape == Caseless.Escape &&
    disregardCase CaseSensitive.FileSeparator == Caseless.FileSeparator &&
    disregardCase CaseSensitive.GroupSeparator == Caseless.GroupSeparator &&
    disregardCase CaseSensitive.RecordSeparator == Caseless.RecordSeparator &&
    disregardCase CaseSensitive.UnitSeparator == Caseless.UnitSeparator &&
    disregardCase CaseSensitive.Space == Caseless.Space &&
    disregardCase CaseSensitive.ExclamationMark == Caseless.ExclamationMark &&
    disregardCase CaseSensitive.QuotationMark == Caseless.QuotationMark &&
    disregardCase CaseSensitive.NumberSign == Caseless.NumberSign &&
    disregardCase CaseSensitive.DollarSign == Caseless.DollarSign &&
    disregardCase CaseSensitive.PercentSign == Caseless.PercentSign &&
    disregardCase CaseSensitive.Ampersand == Caseless.Ampersand &&
    disregardCase CaseSensitive.Apostrophe == Caseless.Apostrophe &&
    disregardCase CaseSensitive.LeftParenthesis == Caseless.LeftParenthesis &&
    disregardCase CaseSensitive.RightParenthesis == Caseless.RightParenthesis &&
    disregardCase CaseSensitive.Asterisk == Caseless.Asterisk &&
    disregardCase CaseSensitive.PlusSign == Caseless.PlusSign &&
    disregardCase CaseSensitive.Comma == Caseless.Comma &&
    disregardCase CaseSensitive.HyphenMinus == Caseless.HyphenMinus &&
    disregardCase CaseSensitive.FullStop == Caseless.FullStop &&
    disregardCase CaseSensitive.Slash == Caseless.Slash &&
    disregardCase CaseSensitive.Digit0 == Caseless.Digit0 &&
    disregardCase CaseSensitive.Digit1 == Caseless.Digit1 &&
    disregardCase CaseSensitive.Digit2 == Caseless.Digit2 &&
    disregardCase CaseSensitive.Digit3 == Caseless.Digit3 &&
    disregardCase CaseSensitive.Digit4 == Caseless.Digit4 &&
    disregardCase CaseSensitive.Digit5 == Caseless.Digit5 &&
    disregardCase CaseSensitive.Digit6 == Caseless.Digit6 &&
    disregardCase CaseSensitive.Digit7 == Caseless.Digit7 &&
    disregardCase CaseSensitive.Digit8 == Caseless.Digit8 &&
    disregardCase CaseSensitive.Digit9 == Caseless.Digit9 &&
    disregardCase CaseSensitive.Colon == Caseless.Colon &&
    disregardCase CaseSensitive.Semicolon == Caseless.Semicolon &&
    disregardCase CaseSensitive.LessThanSign == Caseless.LessThanSign &&
    disregardCase CaseSensitive.EqualsSign == Caseless.EqualsSign &&
    disregardCase CaseSensitive.GreaterThanSign == Caseless.GreaterThanSign &&
    disregardCase CaseSensitive.QuestionMark == Caseless.QuestionMark &&
    disregardCase CaseSensitive.AtSign == Caseless.AtSign &&
    disregardCase CaseSensitive.CapitalLetterA == Caseless.LetterA &&
    disregardCase CaseSensitive.CapitalLetterB == Caseless.LetterB &&
    disregardCase CaseSensitive.CapitalLetterC == Caseless.LetterC &&
    disregardCase CaseSensitive.CapitalLetterD == Caseless.LetterD &&
    disregardCase CaseSensitive.CapitalLetterE == Caseless.LetterE &&
    disregardCase CaseSensitive.CapitalLetterF == Caseless.LetterF &&
    disregardCase CaseSensitive.CapitalLetterG == Caseless.LetterG &&
    disregardCase CaseSensitive.CapitalLetterH == Caseless.LetterH &&
    disregardCase CaseSensitive.CapitalLetterI == Caseless.LetterI &&
    disregardCase CaseSensitive.CapitalLetterJ == Caseless.LetterJ &&
    disregardCase CaseSensitive.CapitalLetterK == Caseless.LetterK &&
    disregardCase CaseSensitive.CapitalLetterL == Caseless.LetterL &&
    disregardCase CaseSensitive.CapitalLetterM == Caseless.LetterM &&
    disregardCase CaseSensitive.CapitalLetterN == Caseless.LetterN &&
    disregardCase CaseSensitive.CapitalLetterO == Caseless.LetterO &&
    disregardCase CaseSensitive.CapitalLetterP == Caseless.LetterP &&
    disregardCase CaseSensitive.CapitalLetterQ == Caseless.LetterQ &&
    disregardCase CaseSensitive.CapitalLetterR == Caseless.LetterR &&
    disregardCase CaseSensitive.CapitalLetterS == Caseless.LetterS &&
    disregardCase CaseSensitive.CapitalLetterT == Caseless.LetterT &&
    disregardCase CaseSensitive.CapitalLetterU == Caseless.LetterU &&
    disregardCase CaseSensitive.CapitalLetterV == Caseless.LetterV &&
    disregardCase CaseSensitive.CapitalLetterW == Caseless.LetterW &&
    disregardCase CaseSensitive.CapitalLetterX == Caseless.LetterX &&
    disregardCase CaseSensitive.CapitalLetterY == Caseless.LetterY &&
    disregardCase CaseSensitive.CapitalLetterZ == Caseless.LetterZ &&
    disregardCase CaseSensitive.LeftSquareBracket == Caseless.LeftSquareBracket &&
    disregardCase CaseSensitive.Backslash == Caseless.Backslash &&
    disregardCase CaseSensitive.RightSquareBracket == Caseless.RightSquareBracket &&
    disregardCase CaseSensitive.Caret == Caseless.Caret &&
    disregardCase CaseSensitive.Underscore == Caseless.Underscore &&
    disregardCase CaseSensitive.GraveAccent == Caseless.GraveAccent &&
    disregardCase CaseSensitive.SmallLetterA == Caseless.LetterA &&
    disregardCase CaseSensitive.SmallLetterB == Caseless.LetterB &&
    disregardCase CaseSensitive.SmallLetterC == Caseless.LetterC &&
    disregardCase CaseSensitive.SmallLetterD == Caseless.LetterD &&
    disregardCase CaseSensitive.SmallLetterE == Caseless.LetterE &&
    disregardCase CaseSensitive.SmallLetterF == Caseless.LetterF &&
    disregardCase CaseSensitive.SmallLetterG == Caseless.LetterG &&
    disregardCase CaseSensitive.SmallLetterH == Caseless.LetterH &&
    disregardCase CaseSensitive.SmallLetterI == Caseless.LetterI &&
    disregardCase CaseSensitive.SmallLetterJ == Caseless.LetterJ &&
    disregardCase CaseSensitive.SmallLetterK == Caseless.LetterK &&
    disregardCase CaseSensitive.SmallLetterL == Caseless.LetterL &&
    disregardCase CaseSensitive.SmallLetterM == Caseless.LetterM &&
    disregardCase CaseSensitive.SmallLetterN == Caseless.LetterN &&
    disregardCase CaseSensitive.SmallLetterO == Caseless.LetterO &&
    disregardCase CaseSensitive.SmallLetterP == Caseless.LetterP &&
    disregardCase CaseSensitive.SmallLetterQ == Caseless.LetterQ &&
    disregardCase CaseSensitive.SmallLetterR == Caseless.LetterR &&
    disregardCase CaseSensitive.SmallLetterS == Caseless.LetterS &&
    disregardCase CaseSensitive.SmallLetterT == Caseless.LetterT &&
    disregardCase CaseSensitive.SmallLetterU == Caseless.LetterU &&
    disregardCase CaseSensitive.SmallLetterV == Caseless.LetterV &&
    disregardCase CaseSensitive.SmallLetterW == Caseless.LetterW &&
    disregardCase CaseSensitive.SmallLetterX == Caseless.LetterX &&
    disregardCase CaseSensitive.SmallLetterY == Caseless.LetterY &&
    disregardCase CaseSensitive.SmallLetterZ == Caseless.LetterZ &&
    disregardCase CaseSensitive.LeftCurlyBracket == Caseless.LeftCurlyBracket &&
    disregardCase CaseSensitive.VerticalLine == Caseless.VerticalLine &&
    disregardCase CaseSensitive.RightCurlyBracket == Caseless.RightCurlyBracket &&
    disregardCase CaseSensitive.Tilde == Caseless.Tilde &&
    disregardCase CaseSensitive.Delete == Caseless.Delete

testToUpper :: Bool
testToUpper =
    toCase UpperCase Caseless.Null == CaseSensitive.Null &&
    toCase UpperCase Caseless.StartOfHeading == CaseSensitive.StartOfHeading &&
    toCase UpperCase Caseless.StartOfText == CaseSensitive.StartOfText &&
    toCase UpperCase Caseless.EndOfText == CaseSensitive.EndOfText &&
    toCase UpperCase Caseless.EndOfTransmission == CaseSensitive.EndOfTransmission &&
    toCase UpperCase Caseless.Enquiry == CaseSensitive.Enquiry &&
    toCase UpperCase Caseless.Acknowledgement == CaseSensitive.Acknowledgement &&
    toCase UpperCase Caseless.Bell == CaseSensitive.Bell &&
    toCase UpperCase Caseless.Backspace == CaseSensitive.Backspace &&
    toCase UpperCase Caseless.HorizontalTab == CaseSensitive.HorizontalTab &&
    toCase UpperCase Caseless.LineFeed == CaseSensitive.LineFeed &&
    toCase UpperCase Caseless.VerticalTab == CaseSensitive.VerticalTab &&
    toCase UpperCase Caseless.FormFeed == CaseSensitive.FormFeed &&
    toCase UpperCase Caseless.CarriageReturn == CaseSensitive.CarriageReturn &&
    toCase UpperCase Caseless.ShiftOut == CaseSensitive.ShiftOut &&
    toCase UpperCase Caseless.ShiftIn == CaseSensitive.ShiftIn &&
    toCase UpperCase Caseless.DataLinkEscape == CaseSensitive.DataLinkEscape &&
    toCase UpperCase Caseless.DeviceControl1 == CaseSensitive.DeviceControl1 &&
    toCase UpperCase Caseless.DeviceControl2 == CaseSensitive.DeviceControl2 &&
    toCase UpperCase Caseless.DeviceControl3 == CaseSensitive.DeviceControl3 &&
    toCase UpperCase Caseless.DeviceControl4 == CaseSensitive.DeviceControl4 &&
    toCase UpperCase Caseless.NegativeAcknowledgement == CaseSensitive.NegativeAcknowledgement &&
    toCase UpperCase Caseless.SynchronousIdle == CaseSensitive.SynchronousIdle &&
    toCase UpperCase Caseless.EndOfTransmissionBlock == CaseSensitive.EndOfTransmissionBlock &&
    toCase UpperCase Caseless.Cancel == CaseSensitive.Cancel &&
    toCase UpperCase Caseless.EndOfMedium == CaseSensitive.EndOfMedium &&
    toCase UpperCase Caseless.Substitute == CaseSensitive.Substitute &&
    toCase UpperCase Caseless.Escape == CaseSensitive.Escape &&
    toCase UpperCase Caseless.FileSeparator == CaseSensitive.FileSeparator &&
    toCase UpperCase Caseless.GroupSeparator == CaseSensitive.GroupSeparator &&
    toCase UpperCase Caseless.RecordSeparator == CaseSensitive.RecordSeparator &&
    toCase UpperCase Caseless.UnitSeparator == CaseSensitive.UnitSeparator &&
    toCase UpperCase Caseless.Space == CaseSensitive.Space &&
    toCase UpperCase Caseless.ExclamationMark == CaseSensitive.ExclamationMark &&
    toCase UpperCase Caseless.QuotationMark == CaseSensitive.QuotationMark &&
    toCase UpperCase Caseless.NumberSign == CaseSensitive.NumberSign &&
    toCase UpperCase Caseless.DollarSign == CaseSensitive.DollarSign &&
    toCase UpperCase Caseless.PercentSign == CaseSensitive.PercentSign &&
    toCase UpperCase Caseless.Ampersand == CaseSensitive.Ampersand &&
    toCase UpperCase Caseless.Apostrophe == CaseSensitive.Apostrophe &&
    toCase UpperCase Caseless.LeftParenthesis == CaseSensitive.LeftParenthesis &&
    toCase UpperCase Caseless.RightParenthesis == CaseSensitive.RightParenthesis &&
    toCase UpperCase Caseless.Asterisk == CaseSensitive.Asterisk &&
    toCase UpperCase Caseless.PlusSign == CaseSensitive.PlusSign &&
    toCase UpperCase Caseless.Comma == CaseSensitive.Comma &&
    toCase UpperCase Caseless.HyphenMinus == CaseSensitive.HyphenMinus &&
    toCase UpperCase Caseless.FullStop == CaseSensitive.FullStop &&
    toCase UpperCase Caseless.Slash == CaseSensitive.Slash &&
    toCase UpperCase Caseless.Digit0 == CaseSensitive.Digit0 &&
    toCase UpperCase Caseless.Digit1 == CaseSensitive.Digit1 &&
    toCase UpperCase Caseless.Digit2 == CaseSensitive.Digit2 &&
    toCase UpperCase Caseless.Digit3 == CaseSensitive.Digit3 &&
    toCase UpperCase Caseless.Digit4 == CaseSensitive.Digit4 &&
    toCase UpperCase Caseless.Digit5 == CaseSensitive.Digit5 &&
    toCase UpperCase Caseless.Digit6 == CaseSensitive.Digit6 &&
    toCase UpperCase Caseless.Digit7 == CaseSensitive.Digit7 &&
    toCase UpperCase Caseless.Digit8 == CaseSensitive.Digit8 &&
    toCase UpperCase Caseless.Digit9 == CaseSensitive.Digit9 &&
    toCase UpperCase Caseless.Colon == CaseSensitive.Colon &&
    toCase UpperCase Caseless.Semicolon == CaseSensitive.Semicolon &&
    toCase UpperCase Caseless.LessThanSign == CaseSensitive.LessThanSign &&
    toCase UpperCase Caseless.EqualsSign == CaseSensitive.EqualsSign &&
    toCase UpperCase Caseless.GreaterThanSign == CaseSensitive.GreaterThanSign &&
    toCase UpperCase Caseless.QuestionMark == CaseSensitive.QuestionMark &&
    toCase UpperCase Caseless.AtSign == CaseSensitive.AtSign &&
    toCase UpperCase Caseless.LetterA == CaseSensitive.CapitalLetterA &&
    toCase UpperCase Caseless.LetterB == CaseSensitive.CapitalLetterB &&
    toCase UpperCase Caseless.LetterC == CaseSensitive.CapitalLetterC &&
    toCase UpperCase Caseless.LetterD == CaseSensitive.CapitalLetterD &&
    toCase UpperCase Caseless.LetterE == CaseSensitive.CapitalLetterE &&
    toCase UpperCase Caseless.LetterF == CaseSensitive.CapitalLetterF &&
    toCase UpperCase Caseless.LetterG == CaseSensitive.CapitalLetterG &&
    toCase UpperCase Caseless.LetterH == CaseSensitive.CapitalLetterH &&
    toCase UpperCase Caseless.LetterI == CaseSensitive.CapitalLetterI &&
    toCase UpperCase Caseless.LetterJ == CaseSensitive.CapitalLetterJ &&
    toCase UpperCase Caseless.LetterK == CaseSensitive.CapitalLetterK &&
    toCase UpperCase Caseless.LetterL == CaseSensitive.CapitalLetterL &&
    toCase UpperCase Caseless.LetterM == CaseSensitive.CapitalLetterM &&
    toCase UpperCase Caseless.LetterN == CaseSensitive.CapitalLetterN &&
    toCase UpperCase Caseless.LetterO == CaseSensitive.CapitalLetterO &&
    toCase UpperCase Caseless.LetterP == CaseSensitive.CapitalLetterP &&
    toCase UpperCase Caseless.LetterQ == CaseSensitive.CapitalLetterQ &&
    toCase UpperCase Caseless.LetterR == CaseSensitive.CapitalLetterR &&
    toCase UpperCase Caseless.LetterS == CaseSensitive.CapitalLetterS &&
    toCase UpperCase Caseless.LetterT == CaseSensitive.CapitalLetterT &&
    toCase UpperCase Caseless.LetterU == CaseSensitive.CapitalLetterU &&
    toCase UpperCase Caseless.LetterV == CaseSensitive.CapitalLetterV &&
    toCase UpperCase Caseless.LetterW == CaseSensitive.CapitalLetterW &&
    toCase UpperCase Caseless.LetterX == CaseSensitive.CapitalLetterX &&
    toCase UpperCase Caseless.LetterY == CaseSensitive.CapitalLetterY &&
    toCase UpperCase Caseless.LetterZ == CaseSensitive.CapitalLetterZ &&
    toCase UpperCase Caseless.LeftSquareBracket == CaseSensitive.LeftSquareBracket &&
    toCase UpperCase Caseless.Backslash == CaseSensitive.Backslash &&
    toCase UpperCase Caseless.RightSquareBracket == CaseSensitive.RightSquareBracket &&
    toCase UpperCase Caseless.Caret == CaseSensitive.Caret &&
    toCase UpperCase Caseless.Underscore == CaseSensitive.Underscore &&
    toCase UpperCase Caseless.GraveAccent == CaseSensitive.GraveAccent &&
    toCase UpperCase Caseless.LeftCurlyBracket == CaseSensitive.LeftCurlyBracket &&
    toCase UpperCase Caseless.VerticalLine == CaseSensitive.VerticalLine &&
    toCase UpperCase Caseless.RightCurlyBracket == CaseSensitive.RightCurlyBracket &&
    toCase UpperCase Caseless.Tilde == CaseSensitive.Tilde &&
    toCase UpperCase Caseless.Delete == CaseSensitive.Delete

testToLower :: Bool
testToLower =
    toCase LowerCase Caseless.Null == CaseSensitive.Null &&
    toCase LowerCase Caseless.StartOfHeading == CaseSensitive.StartOfHeading &&
    toCase LowerCase Caseless.StartOfText == CaseSensitive.StartOfText &&
    toCase LowerCase Caseless.EndOfText == CaseSensitive.EndOfText &&
    toCase LowerCase Caseless.EndOfTransmission == CaseSensitive.EndOfTransmission &&
    toCase LowerCase Caseless.Enquiry == CaseSensitive.Enquiry &&
    toCase LowerCase Caseless.Acknowledgement == CaseSensitive.Acknowledgement &&
    toCase LowerCase Caseless.Bell == CaseSensitive.Bell &&
    toCase LowerCase Caseless.Backspace == CaseSensitive.Backspace &&
    toCase LowerCase Caseless.HorizontalTab == CaseSensitive.HorizontalTab &&
    toCase LowerCase Caseless.LineFeed == CaseSensitive.LineFeed &&
    toCase LowerCase Caseless.VerticalTab == CaseSensitive.VerticalTab &&
    toCase LowerCase Caseless.FormFeed == CaseSensitive.FormFeed &&
    toCase LowerCase Caseless.CarriageReturn == CaseSensitive.CarriageReturn &&
    toCase LowerCase Caseless.ShiftOut == CaseSensitive.ShiftOut &&
    toCase LowerCase Caseless.ShiftIn == CaseSensitive.ShiftIn &&
    toCase LowerCase Caseless.DataLinkEscape == CaseSensitive.DataLinkEscape &&
    toCase LowerCase Caseless.DeviceControl1 == CaseSensitive.DeviceControl1 &&
    toCase LowerCase Caseless.DeviceControl2 == CaseSensitive.DeviceControl2 &&
    toCase LowerCase Caseless.DeviceControl3 == CaseSensitive.DeviceControl3 &&
    toCase LowerCase Caseless.DeviceControl4 == CaseSensitive.DeviceControl4 &&
    toCase LowerCase Caseless.NegativeAcknowledgement == CaseSensitive.NegativeAcknowledgement &&
    toCase LowerCase Caseless.SynchronousIdle == CaseSensitive.SynchronousIdle &&
    toCase LowerCase Caseless.EndOfTransmissionBlock == CaseSensitive.EndOfTransmissionBlock &&
    toCase LowerCase Caseless.Cancel == CaseSensitive.Cancel &&
    toCase LowerCase Caseless.EndOfMedium == CaseSensitive.EndOfMedium &&
    toCase LowerCase Caseless.Substitute == CaseSensitive.Substitute &&
    toCase LowerCase Caseless.Escape == CaseSensitive.Escape &&
    toCase LowerCase Caseless.FileSeparator == CaseSensitive.FileSeparator &&
    toCase LowerCase Caseless.GroupSeparator == CaseSensitive.GroupSeparator &&
    toCase LowerCase Caseless.RecordSeparator == CaseSensitive.RecordSeparator &&
    toCase LowerCase Caseless.UnitSeparator == CaseSensitive.UnitSeparator &&
    toCase LowerCase Caseless.Space == CaseSensitive.Space &&
    toCase LowerCase Caseless.ExclamationMark == CaseSensitive.ExclamationMark &&
    toCase LowerCase Caseless.QuotationMark == CaseSensitive.QuotationMark &&
    toCase LowerCase Caseless.NumberSign == CaseSensitive.NumberSign &&
    toCase LowerCase Caseless.DollarSign == CaseSensitive.DollarSign &&
    toCase LowerCase Caseless.PercentSign == CaseSensitive.PercentSign &&
    toCase LowerCase Caseless.Ampersand == CaseSensitive.Ampersand &&
    toCase LowerCase Caseless.Apostrophe == CaseSensitive.Apostrophe &&
    toCase LowerCase Caseless.LeftParenthesis == CaseSensitive.LeftParenthesis &&
    toCase LowerCase Caseless.RightParenthesis == CaseSensitive.RightParenthesis &&
    toCase LowerCase Caseless.Asterisk == CaseSensitive.Asterisk &&
    toCase LowerCase Caseless.PlusSign == CaseSensitive.PlusSign &&
    toCase LowerCase Caseless.Comma == CaseSensitive.Comma &&
    toCase LowerCase Caseless.HyphenMinus == CaseSensitive.HyphenMinus &&
    toCase LowerCase Caseless.FullStop == CaseSensitive.FullStop &&
    toCase LowerCase Caseless.Slash == CaseSensitive.Slash &&
    toCase LowerCase Caseless.Digit0 == CaseSensitive.Digit0 &&
    toCase LowerCase Caseless.Digit1 == CaseSensitive.Digit1 &&
    toCase LowerCase Caseless.Digit2 == CaseSensitive.Digit2 &&
    toCase LowerCase Caseless.Digit3 == CaseSensitive.Digit3 &&
    toCase LowerCase Caseless.Digit4 == CaseSensitive.Digit4 &&
    toCase LowerCase Caseless.Digit5 == CaseSensitive.Digit5 &&
    toCase LowerCase Caseless.Digit6 == CaseSensitive.Digit6 &&
    toCase LowerCase Caseless.Digit7 == CaseSensitive.Digit7 &&
    toCase LowerCase Caseless.Digit8 == CaseSensitive.Digit8 &&
    toCase LowerCase Caseless.Digit9 == CaseSensitive.Digit9 &&
    toCase LowerCase Caseless.Colon == CaseSensitive.Colon &&
    toCase LowerCase Caseless.Semicolon == CaseSensitive.Semicolon &&
    toCase LowerCase Caseless.LessThanSign == CaseSensitive.LessThanSign &&
    toCase LowerCase Caseless.EqualsSign == CaseSensitive.EqualsSign &&
    toCase LowerCase Caseless.GreaterThanSign == CaseSensitive.GreaterThanSign &&
    toCase LowerCase Caseless.QuestionMark == CaseSensitive.QuestionMark &&
    toCase LowerCase Caseless.AtSign == CaseSensitive.AtSign &&
    toCase LowerCase Caseless.LetterA == CaseSensitive.SmallLetterA &&
    toCase LowerCase Caseless.LetterB == CaseSensitive.SmallLetterB &&
    toCase LowerCase Caseless.LetterC == CaseSensitive.SmallLetterC &&
    toCase LowerCase Caseless.LetterD == CaseSensitive.SmallLetterD &&
    toCase LowerCase Caseless.LetterE == CaseSensitive.SmallLetterE &&
    toCase LowerCase Caseless.LetterF == CaseSensitive.SmallLetterF &&
    toCase LowerCase Caseless.LetterG == CaseSensitive.SmallLetterG &&
    toCase LowerCase Caseless.LetterH == CaseSensitive.SmallLetterH &&
    toCase LowerCase Caseless.LetterI == CaseSensitive.SmallLetterI &&
    toCase LowerCase Caseless.LetterJ == CaseSensitive.SmallLetterJ &&
    toCase LowerCase Caseless.LetterK == CaseSensitive.SmallLetterK &&
    toCase LowerCase Caseless.LetterL == CaseSensitive.SmallLetterL &&
    toCase LowerCase Caseless.LetterM == CaseSensitive.SmallLetterM &&
    toCase LowerCase Caseless.LetterN == CaseSensitive.SmallLetterN &&
    toCase LowerCase Caseless.LetterO == CaseSensitive.SmallLetterO &&
    toCase LowerCase Caseless.LetterP == CaseSensitive.SmallLetterP &&
    toCase LowerCase Caseless.LetterQ == CaseSensitive.SmallLetterQ &&
    toCase LowerCase Caseless.LetterR == CaseSensitive.SmallLetterR &&
    toCase LowerCase Caseless.LetterS == CaseSensitive.SmallLetterS &&
    toCase LowerCase Caseless.LetterT == CaseSensitive.SmallLetterT &&
    toCase LowerCase Caseless.LetterU == CaseSensitive.SmallLetterU &&
    toCase LowerCase Caseless.LetterV == CaseSensitive.SmallLetterV &&
    toCase LowerCase Caseless.LetterW == CaseSensitive.SmallLetterW &&
    toCase LowerCase Caseless.LetterX == CaseSensitive.SmallLetterX &&
    toCase LowerCase Caseless.LetterY == CaseSensitive.SmallLetterY &&
    toCase LowerCase Caseless.LetterZ == CaseSensitive.SmallLetterZ &&
    toCase LowerCase Caseless.LeftSquareBracket == CaseSensitive.LeftSquareBracket &&
    toCase LowerCase Caseless.Backslash == CaseSensitive.Backslash &&
    toCase LowerCase Caseless.RightSquareBracket == CaseSensitive.RightSquareBracket &&
    toCase LowerCase Caseless.Caret == CaseSensitive.Caret &&
    toCase LowerCase Caseless.Underscore == CaseSensitive.Underscore &&
    toCase LowerCase Caseless.GraveAccent == CaseSensitive.GraveAccent &&
    toCase LowerCase Caseless.LeftCurlyBracket == CaseSensitive.LeftCurlyBracket &&
    toCase LowerCase Caseless.VerticalLine == CaseSensitive.VerticalLine &&
    toCase LowerCase Caseless.RightCurlyBracket == CaseSensitive.RightCurlyBracket &&
    toCase LowerCase Caseless.Tilde == CaseSensitive.Tilde &&
    toCase LowerCase Caseless.Delete == CaseSensitive.Delete

dieIfFailures :: Failures a -> IO a
dieIfFailures (Failures fs x) =
    if null fs
        then do putStrLn "💯"; return x
        else die $ intercalate " " (map (("🔥" <> ) . show) fs)

type TestNumber = Natural

test :: TestNumber -> Bool -> Failures ()
test n t = Failures (if t then [] else [n]) ()

data Failures a = Failures [TestNumber] a

instance Functor Failures
  where
    fmap f (Failures a x) = Failures a (f x)

instance Applicative Failures
  where
    pure x = Failures [] x
    Failures a f <*> Failures b x = Failures (a <> b) (f x)

instance Monad Failures
  where
    Failures a x >>= f = let Failures b y = f x in Failures (a <> b) y
