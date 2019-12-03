{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ImplicitParams, LambdaCase, QuasiQuotes, TypeApplications #-}

import qualified ASCII
import ASCII.QQ

import qualified Data.Char as Unicode
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Functor.Identity as I
import qualified System.Exit as Exit
import qualified Data.IORef as Ref
import qualified GHC.Stack as Stack

tests :: (?failed :: Failed) => IO ()
tests =
  do
    ASCII.decodeCharSub @Int 0 === ASCII.Null
    ASCII.decodeCharSub @Int 65 === ASCII.CapitalLetterA
    ASCII.decodeCharSub @Int 97 === ASCII.SmallLetterA
    ASCII.decodeCharSub @Int 127 === ASCII.Delete

    ASCII.decodeCharSub @Int (-1) === ASCII.Substitute
    ASCII.decodeCharSub @Int 128 === ASCII.Substitute
    ASCII.decodeCharSub @Int 11243228 === ASCII.Substitute

    ASCII.decodeChar @Int 0 === Just ASCII.Null
    ASCII.decodeChar @Int 65 === Just ASCII.CapitalLetterA
    ASCII.decodeChar @Int 97 === Just ASCII.SmallLetterA
    ASCII.decodeChar @Int 127 === Just ASCII.Delete

    ASCII.decodeChar @Int (-1) === Nothing
    ASCII.decodeChar @Int 128 === Nothing
    ASCII.decodeChar @Int 11243228 === Nothing

    [ascii|Cat|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT]

    ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAT|] === True
    ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAP|] === False

    ASCII.toCase ASCII.UpperCase ASCII.SmallLetterX === ASCII.CapitalLetterX
    ASCII.toCase ASCII.LowerCase ASCII.SmallLetterX === ASCII.SmallLetterX
    ASCII.toCase ASCII.UpperCase ASCII.CapitalLetterX === ASCII.CapitalLetterX
    ASCII.toCase ASCII.LowerCase ASCII.CapitalLetterX === ASCII.SmallLetterX

    -- Changing the case of an exclamation mark character has no effect because it is not a letter.
    ASCII.toCase ASCII.LowerCase ASCII.ExclamationMark === ASCII.ExclamationMark

    -- Convert "Cat!" to upper case, and you get "CAT!".
    ASCII.toCase ASCII.UpperCase [ascii|Cat!|] === [ascii|CAT!|]

    -- Convert "Cat!" to lower case, and you get "cat!".
    ASCII.toCase ASCII.LowerCase [ascii|Cat!|] === [ascii|cat!|]

    -- Small letter A (a) is lower case.
    ASCII.letterCase ASCII.SmallLetterA === Just ASCII.LowerCase

    -- Capital letter A (A) is upper case.
    ASCII.letterCase ASCII.CapitalLetterA === Just ASCII.UpperCase

    -- The exclamation mark character does not have a case because it is not a letter.
    ASCII.letterCase ASCII.ExclamationMark === Nothing

    -- There are 128 characters in total.
    let allChars = [minBound .. maxBound]
    length allChars === 128

    -- There are 33 control codes.
    let controlCodes = filter (ASCII.inGroup ASCII.Control) allChars
    length controlCodes === 33

    -- There are 95 printable characters.
    let printChars = filter (ASCII.inGroup ASCII.Printable) allChars
    length printChars === 95

    -- Space is a printable character (perhaps surprisingly, given that it is invisible).
    ASCII.charGroup ASCII.Space === ASCII.Printable

    -- Tab is a control code (perhaps surprisingly, given that space is a printable character).
    ASCII.charGroup ASCII.HorizontalTab === ASCII.Control

    -- Space is the first printable character.
    let firstPrintChar = Foldable.minimumBy (compare `Function.on` ASCII.encodeChar @Int) printChars
    firstPrintChar === ASCII.Space

    -- Small letter A (a) is a printable character.
    ASCII.charGroup ASCII.SmallLetterA === ASCII.Printable

    -- Tilde (~) is a printable character.
    ASCII.charGroup ASCII.Tilde === ASCII.Printable

    -- Tilde is the last printable character.
    let lastPrintChar = Foldable.maximumBy (compare `Function.on` ASCII.encodeChar @Int) printChars
    lastPrintChar === ASCII.Tilde

    -- Null is the first character.
    minBound === ASCII.Null

    -- Null is a control code.
    ASCII.charGroup ASCII.Null === ASCII.Control

    -- UnitSeparator is a control code.
    ASCII.charGroup ASCII.UnitSeparator === ASCII.Control

    -- UnitSeparator is the last control code that appears in the ASCII chart before the printable characters.
    let lastControlCharBeforePrint = Foldable.maximumBy (compare `Function.on` ASCII.encodeChar @Int) (takeWhile (ASCII.inGroup ASCII.Control) allChars)
    lastControlCharBeforePrint === ASCII.UnitSeparator

    -- Delete is the last character.
    maxBound === ASCII.Delete

    -- Delete is a control code.
    ASCII.charGroup ASCII.Delete === ASCII.Control

    -- Delete is the only control code that appears in the ASCII chart /after/ the printable characters.
    let afterPrintChars = (dropWhile (ASCII.inGroup ASCII.Printable) . dropWhile (ASCII.inGroup ASCII.Control)) allChars
    afterPrintChars === [ASCII.Delete]

    -- The Show output for [ascii|cat|] is: ASCII.fromUnicodeSub "cat" (In this test case, the quotes are escaped, so it's a bit difficult to read.)
    show [ascii|cat|] === "ASCII.fromUnicodeSub \"cat\""

    -- The Show output for the empty string [ascii||] is: ASCII.fromUnicodeSub ""
    show [ascii||] === "ASCII.fromUnicodeSub \"\""

    -- The Show output for a string containing a single quotation mark is: ASCII.fromUnicodeSub "\"" (In this test case, the inner quotation mark is doubly-escaped, so it's especially difficult to read.)
    show [ascii|"|] === "ASCII.fromUnicodeSub \"\\\"\""

    -- The Show instance for ASCII.String adds parens appropriately based on context.
    show (I.Identity [ascii|cat|]) === "Identity (ASCII.fromUnicodeSub \"cat\")"

    Foldable.for_ classificationFunctions $ \(f, g) ->
      Foldable.for_ allChars $ \c ->
        f c === g (ASCII.toUnicode c)

classificationFunctions :: [(ASCII.Char -> Bool, Unicode.Char -> Bool)]
classificationFunctions =
    [ (ASCII.isControl, Unicode.isControl)
    , (ASCII.isSpace, Unicode.isSpace)
    , (ASCII.isLower, Unicode.isLower)
    , (ASCII.isUpper, Unicode.isUpper)
    , (ASCII.isAlpha, Unicode.isAlpha)
    , (ASCII.isAlphaNum, Unicode.isAlphaNum)
    , (ASCII.isPrint, Unicode.isPrint)
    , (ASCII.isDigit, Unicode.isDigit)
    , (ASCII.isOctDigit, Unicode.isOctDigit)
    , (ASCII.isHexDigit, Unicode.isHexDigit)
    , (ASCII.isLetter, Unicode.isLetter)
    , (ASCII.isMark, Unicode.isMark)
    , (ASCII.isNumber, Unicode.isNumber)
    , (ASCII.isPunctuation, Unicode.isPunctuation)
    , (ASCII.isSymbol, Unicode.isSymbol)
    , (ASCII.isSeparator, Unicode.isSeparator)
    ]

main :: IO ()
main =
  do
    failed <- Ref.newIORef False; let ?failed = failed :: Failed
    tests
    Ref.readIORef failed >>= \case { True -> Exit.exitFailure; False -> Exit.exitSuccess }

type Failed = Ref.IORef Bool

(===) :: (Stack.HasCallStack, ?failed :: Failed) => (Eq a, Show a) => a -> a -> IO ()
(===) x y | x == y = mempty
(===) x y =
  do
    putStrLn (unwords ["Line", show lineNumber] ++ ": " ++ unwords [show x, "/=", show y])
    Ref.writeIORef ?failed True

lineNumber :: Stack.HasCallStack => Int
lineNumber = (Stack.srcLocStartLine . snd . last . Stack.getCallStack) Stack.callStack
