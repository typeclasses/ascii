{-# LANGUAGE ImplicitParams, LambdaCase #-}

import qualified ASCII
import qualified System.Exit as Exit
import qualified Data.IORef as Ref
import qualified GHC.Stack as Stack

import Data.Foldable (for_)

main :: IO ()
main =
  do
    failed <- Ref.newIORef False
    let ?failed = failed

    ASCII.intChar 0 === ASCII.Null
    ASCII.intChar 65 === ASCII.CapitalLetterA
    ASCII.intChar 97 === ASCII.SmallLetterA
    ASCII.intChar 127 === ASCII.Delete

    ASCII.intChar (-1) === ASCII.Substitute
    ASCII.intChar 128 === ASCII.Substitute
    ASCII.intChar 11243228 === ASCII.Substitute

    ASCII.intCharMaybe 0 === Just ASCII.Null
    ASCII.intCharMaybe 65 === Just ASCII.CapitalLetterA
    ASCII.intCharMaybe 97 === Just ASCII.SmallLetterA
    ASCII.intCharMaybe 127 === Just ASCII.Delete

    ASCII.intCharMaybe (-1) === Nothing
    ASCII.intCharMaybe 128 === Nothing
    ASCII.intCharMaybe 11243228 === Nothing

    minBound === ASCII.Null
    maxBound === ASCII.Delete

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
