{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, QuasiQuotes, TypeApplications #-}

import qualified ASCII
import ASCII.QQ

import Control.Applicative
import qualified Data.Char as Unicode
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Functor.Identity as I
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified Data.IORef as Ref
import qualified GHC.Stack as Stack

main :: IO ()
main = runTest $ foldl1 (<>) $

  [ ASCII.decodeCharSub @Int 0 === ASCII.Null
  , ASCII.decodeCharSub @Int 65 === ASCII.CapitalLetterA
  , ASCII.decodeCharSub @Int 97 === ASCII.SmallLetterA
  , ASCII.decodeCharSub @Int 127 === ASCII.Delete

  , ASCII.decodeCharSub @Int (-1) === ASCII.Substitute
  , ASCII.decodeCharSub @Int 128 === ASCII.Substitute
  , ASCII.decodeCharSub @Int 11243228 === ASCII.Substitute

  , ASCII.decodeChar @Int 0 === Just ASCII.Null
  , ASCII.decodeChar @Int 65 === Just ASCII.CapitalLetterA
  , ASCII.decodeChar @Int 97 === Just ASCII.SmallLetterA
  , ASCII.decodeChar @Int 127 === Just ASCII.Delete

  , ASCII.decodeChar @Int (-1) === Nothing
  , ASCII.decodeChar @Int 128 === Nothing
  , ASCII.decodeChar @Int 11243228 === Nothing

  , [ascii|Cat|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT]

  , ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAT|] === True
  , ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAP|] === False

  , ASCII.toCase ASCII.UpperCase ASCII.SmallLetterX === ASCII.CapitalLetterX
  , ASCII.toCase ASCII.LowerCase ASCII.SmallLetterX === ASCII.SmallLetterX
  , ASCII.toCase ASCII.UpperCase ASCII.CapitalLetterX === ASCII.CapitalLetterX
  , ASCII.toCase ASCII.LowerCase ASCII.CapitalLetterX === ASCII.SmallLetterX

  -- Changing the case of an exclamation mark character has no effect because it is not a letter.
  , ASCII.toCase ASCII.LowerCase ASCII.ExclamationMark === ASCII.ExclamationMark

  -- Convert "Cat!" to upper case, and you get "CAT!".
  , ASCII.toCase ASCII.UpperCase [ascii|Cat!|] === [ascii|CAT!|]

  -- Convert "Cat!" to lower case, and you get "cat!".
  , ASCII.toCase ASCII.LowerCase [ascii|Cat!|] === [ascii|cat!|]

  -- Small letter A (a) is lower case.
  , ASCII.letterCase ASCII.SmallLetterA === Just ASCII.LowerCase

  -- Capital letter A (A) is upper case.
  , ASCII.letterCase ASCII.CapitalLetterA === Just ASCII.UpperCase

  -- The exclamation mark character does not have a case because it is not a letter.
  , ASCII.letterCase ASCII.ExclamationMark === Nothing

  -- There are 128 characters in total.
  , length allChars === 128

  -- There are 33 control codes.
  , length controlCodes === 33

  -- There are 95 printable characters.
  , length printChars === 95

  -- Space is a printable character (perhaps surprisingly, given that it is invisible).
  , ASCII.charGroup ASCII.Space === ASCII.Printable

  -- Tab is a control code (perhaps surprisingly, given that space is a printable character).
  , ASCII.charGroup ASCII.HorizontalTab === ASCII.Control

  -- Space is the first printable character.
  , firstPrintChar === ASCII.Space

  -- Small letter A (a) is a printable character.
  , ASCII.charGroup ASCII.SmallLetterA === ASCII.Printable

  -- Tilde (~) is a printable character.
  , ASCII.charGroup ASCII.Tilde === ASCII.Printable

  -- Tilde is the last printable character.
  , lastPrintChar === ASCII.Tilde

  -- Null is the first character.
  , minBound === ASCII.Null

  -- Null is a control code.
  , ASCII.charGroup ASCII.Null === ASCII.Control

  -- UnitSeparator is a control code.
  , ASCII.charGroup ASCII.UnitSeparator === ASCII.Control

  -- UnitSeparator is the last control code that appears in the ASCII chart before the printable characters.
  , lastControlCharBeforePrint === ASCII.UnitSeparator

  -- Delete is the last character.
  , maxBound === ASCII.Delete

  -- Delete is a control code.
  , ASCII.charGroup ASCII.Delete === ASCII.Control

  -- Delete is the only control code that appears in the ASCII chart /after/ the printable characters.
  , afterPrintChars === [ASCII.Delete]

  -- The Show output for [ascii|cat|] is: ASCII.fromUnicodeSub "cat" (In this test case, the quotes are escaped, so it's a bit difficult to read.)
  , show [ascii|cat|] === "ASCII.fromUnicodeSub \"cat\""

  -- The Show output for the empty string [ascii||] is: ASCII.fromUnicodeSub ""
  , show [ascii||] === "ASCII.fromUnicodeSub \"\""

  -- The Show output for a string containing a single quotation mark is: ASCII.fromUnicodeSub "\"" (In this test case, the inner quotation mark is doubly-escaped, so it's especially difficult to read.)
  , show [ascii|"|] === "ASCII.fromUnicodeSub \"\\\"\""

    -- The Show instance for ASCII.String adds parens appropriately based on context.
  , show (I.Identity [ascii|cat|]) === "Identity (ASCII.fromUnicodeSub \"cat\")"

  , for classificationFunctions $ \(name, f, g) -> note "f" name $
        for allChars $ \x -> note "x" (show x) $
            f x === g (ASCII.toUnicode x)

  ]

allChars = [minBound .. maxBound]

controlCodes = filter (ASCII.inGroup ASCII.Control) allChars

printChars = filter (ASCII.inGroup ASCII.Printable) allChars

firstPrintChar = Foldable.minimumBy (compare `Function.on` ASCII.encodeChar @Int) printChars

lastPrintChar = Foldable.maximumBy (compare `Function.on` ASCII.encodeChar @Int) printChars

lastControlCharBeforePrint = Foldable.maximumBy (compare `Function.on` ASCII.encodeChar @Int) (takeWhile (ASCII.inGroup ASCII.Control) allChars)

afterPrintChars = (dropWhile (ASCII.inGroup ASCII.Printable) . dropWhile (ASCII.inGroup ASCII.Control)) allChars

classificationFunctions :: [(String, ASCII.Char -> Bool, Unicode.Char -> Bool)]
classificationFunctions =
    [ ("isControl", ASCII.isControl, Unicode.isControl)
    , ("isSpace", ASCII.isSpace, Unicode.isSpace)
    , ("isLower", ASCII.isLower, Unicode.isLower)
    , ("isUpper", ASCII.isUpper, Unicode.isUpper)
    , ("isAlpha", ASCII.isAlpha, Unicode.isAlpha)
    , ("isAlphaNum", ASCII.isAlphaNum, Unicode.isAlphaNum)
    , ("isPrint", ASCII.isPrint, Unicode.isPrint)
    , ("isDigit", ASCII.isDigit, Unicode.isDigit)
    , ("isOctDigit", ASCII.isOctDigit, Unicode.isOctDigit)
    , ("isHexDigit", ASCII.isHexDigit, Unicode.isHexDigit)
    , ("isLetter", ASCII.isLetter, Unicode.isLetter)
    , ("isMark", ASCII.isMark, Unicode.isMark)
    , ("isNumber", ASCII.isNumber, Unicode.isNumber)
    , ("isPunctuation", ASCII.isPunctuation, Unicode.isPunctuation)
    , ("isSymbol", ASCII.isSymbol, Unicode.isSymbol)
    , ("isSeparator", ASCII.isSeparator, Unicode.isSeparator)
    ]

runTest :: Test -> IO ()
runTest (Test f) =
    case f [] of
        [] -> Exit.exitSuccess
        xs -> Foldable.for_ xs putStrLn >> Exit.exitFailure

type Note = (String, String)
type Notes = [Note]
type Failure = String

newtype Test = Test (Notes -> [Failure])

instance Semigroup Test
  where
    Test ta <> Test tb = Test $ \n -> ta n ++ tb n

success :: Test
success = Test $ const []

for :: [a] -> (a -> Test) -> Test
for xs f = foldl1 (<>) (List.map f xs)

note :: String -> String -> Test -> Test
note k v (Test f) = Test $ \n -> f ((k, v) : n)

(===) :: (Stack.HasCallStack) => (Eq a, Show a) => a -> a -> Test
(===) x y | x == y = success
(===) x y = Test $ \ns ->
    [
        List.intercalate "; "
          (
            [unwords ["Line", show lineNumber] :: String] ++
            (map (\(k, v) -> unwords [k, "=", v]) ns :: [String]) ++
            [unwords [show x, "/=", show y] :: String]
            :: [String]
          )
    ]

lineNumber :: Stack.HasCallStack => Int
lineNumber = (Stack.srcLocStartLine . snd . last . Stack.getCallStack) Stack.callStack
