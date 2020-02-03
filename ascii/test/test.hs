{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, QuasiQuotes, TypeApplications #-}

import qualified ASCII

import Prelude hiding ((+))

import qualified Data.Char as Unicode
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Functor.Contravariant as Eq
import qualified Data.Functor.Identity as I
import qualified Data.List as List
import qualified System.Exit as Exit
import qualified GHC.Stack as Stack

main :: IO ()
main = runTest $ let (+) = (<>) in

    do ASCII.toCharSub @Int 0 === ASCII.Null
  + do ASCII.toCharSub @Int 65 === ASCII.CapitalLetterA
  + do ASCII.toCharSub @Int 97 === ASCII.SmallLetterA
  + do ASCII.toCharSub @Int 127 === ASCII.Delete

  + do ASCII.toCharSub @Int (-1) === ASCII.Substitute
  + do ASCII.toCharSub @Int 128 === ASCII.Substitute
  + do ASCII.toCharSub @Int 11243228 === ASCII.Substitute

  + do ASCII.toCharMaybe @Int 0 === Just ASCII.Null
  + do ASCII.toCharMaybe @Int 65 === Just ASCII.CapitalLetterA
  + do ASCII.toCharMaybe @Int 97 === Just ASCII.SmallLetterA
  + do ASCII.toCharMaybe @Int 127 === Just ASCII.Delete

  + do ASCII.toCharMaybe @Int (-1) === Nothing
  + do ASCII.toCharMaybe @Int 128 === Nothing
  + do ASCII.toCharMaybe @Int 11243228 === Nothing

  + do [ASCII.string|Cat|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT]

  + do (==)                     [ASCII.string|Cat|] [ASCII.string|CAT|] === False
  + do ASCII.equalsIgnoringCase [ASCII.string|Cat|] [ASCII.string|CAT|] === True
  + do ASCII.equalsIgnoringCase [ASCII.string|Cat|] [ASCII.string|CAP|] === False
  + do Eq.getEquivalence ASCII.caseInsensitiveEquivalence [ASCII.string|Cat|] [ASCII.string|CAT|] === True
  + do Eq.getEquivalence ASCII.caseInsensitiveEquivalence [ASCII.string|Cat|] [ASCII.string|CAP|] === False

  + do ASCII.toCase ASCII.UpperCase ASCII.SmallLetterX === ASCII.CapitalLetterX
  + do ASCII.toCase ASCII.LowerCase ASCII.SmallLetterX === ASCII.SmallLetterX
  + do ASCII.toCase ASCII.UpperCase ASCII.CapitalLetterX === ASCII.CapitalLetterX
  + do ASCII.toCase ASCII.LowerCase ASCII.CapitalLetterX === ASCII.SmallLetterX

  -- Changing the case of an exclamation mark character has no effect because it is not a letter.
  + do ASCII.toCase ASCII.LowerCase ASCII.ExclamationMark === ASCII.ExclamationMark

  -- Convert "Cat!" to upper case, and you get "CAT!".
  + do ASCII.toCase ASCII.UpperCase [ASCII.string|Cat!|] === [ASCII.string|CAT!|]

  -- Convert "Cat!" to lower case, and you get "cat!".
  + do ASCII.toCase ASCII.LowerCase [ASCII.string|Cat!|] === [ASCII.string|cat!|]

  -- Small letter A (a) is lower case.
  + do ASCII.letterCase ASCII.SmallLetterA === Just ASCII.LowerCase

  -- Capital letter A (A) is upper case.
  + do ASCII.letterCase ASCII.CapitalLetterA === Just ASCII.UpperCase

  -- The exclamation mark character does not have a case because it is not a letter.
  + do ASCII.letterCase ASCII.ExclamationMark === Nothing

  -- There are 128 characters in total.
  + do length ASCII.allCharacters === 128

  -- There are 33 control codes.
  + do length ASCII.controlCodes === 33

  -- There are 95 printable characters.
  + do length ASCII.printableCharacters === 95

  -- Space is a printable character (perhaps surprisingly, given that it is invisible).
  + do ASCII.charGroup ASCII.Space === ASCII.Printable

  -- Tab is a control code (perhaps surprisingly, given that space is a printable character).
  + do ASCII.charGroup ASCII.HorizontalTab === ASCII.Control

  -- Space is the first printable character.
  + do Foldable.minimumBy (compare `Function.on` ASCII.fromChar @Int) ASCII.printableCharacters === ASCII.Space

  -- Small letter A (a) is a printable character.
  + do ASCII.charGroup ASCII.SmallLetterA === ASCII.Printable

  -- Tilde (~) is a printable character.
  + do ASCII.charGroup ASCII.Tilde === ASCII.Printable

  -- Tilde is the last printable character.
  + do Foldable.maximumBy (compare `Function.on` ASCII.fromChar @Int) ASCII.printableCharacters === ASCII.Tilde

  -- Null is the first character.
  + do minBound === ASCII.Null

  -- Null is a control code.
  + do ASCII.charGroup ASCII.Null === ASCII.Control

  -- UnitSeparator is a control code.
  + do ASCII.charGroup ASCII.UnitSeparator === ASCII.Control

  -- UnitSeparator is the last control code that appears in the ASCII chart before the printable characters.
  + do Foldable.maximumBy (compare `Function.on` ASCII.fromChar @Int) (takeWhile (ASCII.inGroup ASCII.Control) ASCII.allCharacters) === ASCII.UnitSeparator

  -- Delete is the last character.
  + do maxBound === ASCII.Delete

  -- Delete is a control code.
  + do ASCII.charGroup ASCII.Delete === ASCII.Control

  -- Delete is the only control code that appears in the ASCII chart /after/ the printable characters.
  + do (dropWhile (ASCII.inGroup ASCII.Printable) . dropWhile (ASCII.inGroup ASCII.Control)) ASCII.allCharacters === [ASCII.Delete]

  -- The Show output for [ASCII.string|cat|] is: ASCII.toStringSub "cat" (In this test case, the quotes are escaped, so it's a bit difficult to read.)
  + do show [ASCII.string|cat|] === "ASCII.toStringSub \"cat\""

  -- The Show output for the empty string [ASCII.string||] is: ASCII.toStringSub ""
  + do show [ASCII.string||] === "ASCII.toStringSub \"\""

  -- The Show output for a string containing a single quotation mark is: ASCII.toStringSub "\"" (In this test case, the inner quotation mark is doubly-escaped, so it's especially difficult to read.)
  + do show [ASCII.string|"|] === "ASCII.toStringSub \"\\\"\""

  -- The Show instance for ASCII.String adds parens appropriately based on context.
  + do show (I.Identity [ASCII.string|cat|]) === "Identity (ASCII.toStringSub \"cat\")"

  -- These are functions under the "classification functions" heading in the Data.Char module that have equivalents in the ASCII module. For every ASCII character, each of the two equivalent functions should yield the same results.
  + do for classificationFunctions $ \(name, f, g) -> for ASCII.allCharacters $ \x -> ("ASCII." ++ name ++ " " ++ show x, f x) =#= ("Data.Char." ++ name ++ " " ++ show x, g (ASCII.fromChar @Unicode.Char x))

  -- These are situations where we present the same information in two different forms: Once as a list of all ASCII characters with some classification, and again as a function that tests whether a particular character belongs to the classification. The list should contain exactly the characters for which the corresponding predicate is true, and all of the lists should be sorted in ascending order.
  + do for listsAndPredicates $ \(name, list, predicate) -> note ("ASCII." ++ name) $ list === filter predicate ASCII.allCharacters

  -- Some very straightforward checks that all the quasi-quote expressions compile and produce the values they're supposed to.
  + do case [ASCII.char|~|] of
           [ASCII.char|@|] -> failure
           [ASCII.char|~|] -> success
           _               -> failure
  + do [ASCII.list|Cat!|] === [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT, ASCII.ExclamationMark]
  + do [ASCII.string|Cat!|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT, ASCII.ExclamationMark]
  + do [ASCII.bytes|Cat!|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT, ASCII.ExclamationMark]

  -- Examples from the "string functions" documentation
  + do ASCII.replicate 5 ASCII.CapitalLetterA === [ASCII.string|AAAAA|]
  + do ASCII.take 3 [ASCII.string|December|] === [ASCII.string|Dec|]
  + do ASCII.drop 3 [ASCII.string|December|] === [ASCII.string|ember|]
  + do ASCII.span ASCII.isLower [ASCII.string|oneTWOthree|] === ([ASCII.string|one|], [ASCII.string|TWOthree|])
  + do ASCII.reverse [ASCII.string|fish|] === [ASCII.string|hsif|]
  + do ASCII.any ASCII.isDigit [ASCII.string|fish|] === False
  + do ASCII.any ASCII.isDigit [ASCII.string|fish2|] === True
  + do ASCII.all ASCII.isLetter [ASCII.string|fish|] === True
  + do ASCII.all ASCII.isLetter [ASCII.string|fish2|] === False
  + do ASCII.append [ASCII.string|One|] [ASCII.string|Two|] === [ASCII.string|OneTwo|]
  + do ASCII.concat [ [ASCII.string|One|], [ASCII.string|Two|], [ASCII.string|Three|] ] === [ASCII.string|OneTwoThree|]
  + do ASCII.cons ASCII.CapitalLetterC [ASCII.string|at|] === [ASCII.string|Cat|]
  + do ASCII.snoc [ASCII.string|Ca|] ASCII.SmallLetterT === [ASCII.string|Cat|]
  + do ASCII.uncons [ASCII.string||] === Nothing
  + do ASCII.uncons [ASCII.string|Cat|] === Just (ASCII.CapitalLetterC, [ASCII.string|at|])

listsAndPredicates :: [(String, [ASCII.Char], ASCII.Char -> Bool)]
listsAndPredicates =
    [ ("controlCodes", ASCII.controlCodes, ASCII.isControl)
    , ("printableCharacters", ASCII.printableCharacters, ASCII.isPrint)
    , ("letters", ASCII.letters, ASCII.isLetter)
    , ("capitalLetters", ASCII.capitalLetters, ASCII.isUpper)
    , ("smallLetters", ASCII.smallLetters, ASCII.isLower)
    , ("digits", ASCII.digits, ASCII.isDigit)
    , ("numbers", ASCII.numbers, ASCII.isNumber)
    , ("octDigits", ASCII.octDigits, ASCII.isOctDigit)
    , ("hexDigits", ASCII.hexDigits, ASCII.isHexDigit)
    ]

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
        [] -> putStrLn "All ASCII tests passed." >> Exit.exitSuccess
        xs -> Foldable.traverse_ putStrLn (List.intersperse "" xs) >> Exit.exitFailure

type Note = String
type Notes = [Note]
type Failure = String
type Failures = [Failure]

newtype Test = Test (Notes -> Failures)

instance Semigroup Test
  where
    Test ta <> Test tb = Test $ \n -> ta n ++ tb n

success :: Test
success = Test $ const []

failure :: Stack.HasCallStack => Test
failure = Test $ \ns -> [List.intercalate "\n" ([unwords ["❌ Detected a problem at line", show lineNumber, "of the test program."]] ++ ns)]

for :: [a] -> (a -> Test) -> Test
for xs f = foldl1 (<>) (List.map f xs)

note :: String -> Test -> Test
note n (Test f) = Test $ \ns -> f (ns ++ [n])

(===) :: Stack.HasCallStack => (Eq a, Show a) => a -> a -> Test
(===) x y | x == y = success
(===) x y =
    note (unwords [show x, "/=", show y]) $
    failure

(=#=) :: Stack.HasCallStack => (Eq a, Show a) => (String, a) -> (String, a) -> Test
(=#=) (_, v1) (_, v2) | v1 == v2 = success
(=#=) (k1, v1) (k2, v2) =
    note (k1 ++ " = " ++ show v1) $
    note (k2 ++ " = " ++ show v2) $
    failure

lineNumber :: Stack.HasCallStack => Int
lineNumber = (Stack.srcLocStartLine . snd . last . Stack.getCallStack) Stack.callStack
