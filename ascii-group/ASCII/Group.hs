module ASCII.Group ( Group (..), charGroup, inGroup ) where

import qualified Prelude
import Prelude ((<), (==), Bool)

import ASCII.Char (Char)
import qualified ASCII.Char as Char

-- | ASCII characters are broadly categorized into two groups: /control codes/ and /printable characters/.

data Group =
    Control -- ^ 33 of the ASCII characters are control codes. A few of these are still in use, but most are obsolete relics of the early days of computing.
  | Printable -- ^ 95 of the ASCII characters are /printable characters/ such as letters and numbers, mostly corresponding to the keys on an American English keyboard.

deriving instance Prelude.Eq Group
deriving instance Prelude.Ord Group
deriving instance Prelude.Enum Group
deriving instance Prelude.Bounded Group
deriving instance Prelude.Show Group

-- | Determine which group a particular character belongs to.
charGroup :: Char -> Group
charGroup x =
    case x of
        _ | (x < Char.Space) -> Control
        Char.Delete          -> Control
        _                    -> Printable

-- | Test whether a character belongs to a particular group.
inGroup :: Group -> Char -> Bool
inGroup g x = charGroup x == g
