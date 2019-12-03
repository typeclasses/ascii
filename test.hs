{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ImplicitParams, LambdaCase, QuasiQuotes, TypeApplications #-}

import qualified ASCII
import ASCII.QQ

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

    minBound === ASCII.Null
    maxBound === ASCII.Delete

    [ascii|Cat|] === ASCII.pack [ASCII.CapitalLetterC, ASCII.SmallLetterA, ASCII.SmallLetterT]

    ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAT|] === True
    ASCII.equalsIgnoringCase [ascii|Cat|] [ascii|CAP|] === False

    ASCII.toCase ASCII.UpperCase ASCII.SmallLetterX === ASCII.CapitalLetterX
    ASCII.toCase ASCII.LowerCase ASCII.SmallLetterX === ASCII.SmallLetterX
    ASCII.toCase ASCII.UpperCase ASCII.CapitalLetterX === ASCII.CapitalLetterX
    ASCII.toCase ASCII.LowerCase ASCII.CapitalLetterX === ASCII.SmallLetterX
    ASCII.toCase ASCII.LowerCase ASCII.ExclamationMark === ASCII.ExclamationMark

    ASCII.toCase ASCII.UpperCase [ascii|Cat|] === [ascii|CAT|]
    ASCII.toCase ASCII.LowerCase [ascii|Cat|] === [ascii|cat|]

    ASCII.letterCase ASCII.SmallLetterA === Just ASCII.LowerCase
    ASCII.letterCase ASCII.CapitalLetterA === Just ASCII.UpperCase
    ASCII.letterCase ASCII.ExclamationMark === Nothing

    ASCII.charGroup ASCII.Null === ASCII.Control
    ASCII.charGroup ASCII.Space === ASCII.Printable
    ASCII.charGroup ASCII.SmallLetterA === ASCII.Printable
    ASCII.charGroup ASCII.Tilde === ASCII.Printable
    ASCII.charGroup ASCII.Delete === ASCII.Control

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
