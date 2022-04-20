module ASCII.Word4 ( Word4 (..) ) where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.Ord (Ord (..))
import GHC.Generics (Generic)
import Prelude (Bounded (..), Enum (..))
import Text.Show (Show)

-- | 4-bit unsigned integer (a whole number between /0/ and /15/)
data Word4
    = Number0  -- ^ Zero
    | Number1  -- ^ One
    | Number2  -- ^ Two
    | Number3  -- ^ Three
    | Number4  -- ^ Four
    | Number5  -- ^ Five
    | Number6  -- ^ Six
    | Number7  -- ^ Seven
    | Number8  -- ^ Eight
    | Number9  -- ^ Nine
    | Number10 -- ^ Ten      (A)
    | Number11 -- ^ Eleven   (B)
    | Number12 -- ^ Twelve   (C)
    | Number13 -- ^ Thirteen (D)
    | Number14 -- ^ Fourteen (E)
    | Number15 -- ^ Fifteen  (F)
    deriving stock (Bounded, Enum, Eq, Ord, Show, Data, Generic)
    deriving anyclass Hashable
