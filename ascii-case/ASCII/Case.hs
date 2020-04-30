module ASCII.Case ( Case (..), letterCase, isCase ) where

import ASCII.Char (Char (..))

import Prelude ((<=), (>=), Bool, Maybe (..), otherwise)
import qualified Prelude
import qualified Data.Bool as Bool

-- | /Case/ is a property of letters. /A-Z/ are /upper case/ letters, and /a-z/ are /lower case/ letters. No other ASCII characters have case.
data Case =
    UpperCase -- ^ The letters from 'CapitalLetterA' to 'CapitalLetterZ'.
  | LowerCase -- ^ The letters from 'SmallLetterA' to 'SmallLetterZ'.

deriving instance Prelude.Eq Case
deriving instance Prelude.Ord Case
deriving instance Prelude.Enum Case
deriving instance Prelude.Bounded Case
deriving instance Prelude.Show Case

-- | Determines whether a character is a letter, and if so, whether it is upper or lower case.
--
-- ==== Examples
--
-- >>> letterCase CapitalLetterR
-- Just UpperCase
--
-- >>> letterCase SmallLetterR
-- Just LowerCase
--
-- >>> letterCase DollarSign
-- Nothing

letterCase :: Char -> Maybe Case
letterCase x | isCase UpperCase x = Just UpperCase
             | isCase LowerCase x = Just LowerCase
             | otherwise          = Nothing

-- | Determines whether a character is a letter of a particular case.
--
-- === Examples
--
-- >>> isCase UpperCase CapitalLetterR
-- True
--
-- >>> isCase UpperCase SmallLetterR
-- False
--
-- >>> isCase UpperCase DollarSign
-- False

isCase :: Case -> Char -> Bool
isCase UpperCase x = (Bool.&&) (x >= CapitalLetterA) (x <= CapitalLetterZ)
isCase LowerCase x = (Bool.&&) (x >= SmallLetterA) (x <= SmallLetterZ)
