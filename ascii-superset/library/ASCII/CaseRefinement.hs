module ASCII.CaseRefinement
  (
    {- * ASCII'case type constructor -} ASCII'case, lift, asciiCaseUnsafe,
    {- ** Aliases -} {- $aliases -} ASCII'upper, ASCII'lower,
  )
  where

import ASCII.Case (Case (..))
import ASCII.Superset (CharSuperset, StringSuperset)

import Data.Bool (Bool (..))
import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function (id, ($), (.))
import Data.Hashable (Hashable)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>))
import Data.Semigroup (Semigroup)
import GHC.Generics (Generic)
import Prelude (succ)
import Text.Show (Show, showList, showParen, showString, showsPrec)

{- | This type constructor indicates that a value from some ASCII superset is valid ASCII, and also that any letters belong to a particular 'Case' indicated by the @letterCase@ type parameter.

The @superset@ type parameter is the ASCII superset, which should be a type with an instance of either 'CharSuperset' or 'StringSuperset'.

For example, whereas a 'Data.Text.Text' value may contain a combination of ASCII and non-ASCII characters, a value of type @'ASCII'case' ''ASCII.Case.UpperCase' 'Data.Text.Text'@ may contain only uppercase ASCII letters and ASCII non-letters.

-}
newtype ASCII'case (letterCase :: Case) superset = ASCII'case_Unsafe
  { lift :: superset
      -- ^ Discard the evidence that the value is known to consist entirely of ASCII characters in a particular case
  }

deriving stock instance Eq superset => Eq (ASCII'case letterCase superset)

deriving stock instance Ord superset => Ord (ASCII'case letterCase superset)

deriving newtype instance Hashable superset => Hashable (ASCII'case letterCase superset)

deriving newtype instance Semigroup superset => Semigroup (ASCII'case letterCase superset)

deriving newtype instance Monoid superset => Monoid (ASCII'case letterCase superset)

deriving stock instance (Data superset, Typeable letterCase) => Data (ASCII'case letterCase superset)

deriving stock instance Generic (ASCII'case letterCase superset)

asciiCaseUnsafe :: superset -> ASCII'case letterCase superset
asciiCaseUnsafe = ASCII'case_Unsafe

{- $aliases

The 'ASCII'upper' and 'ASCII'lower' type aliases exist primarily so that you can use 'ASCII'case' without the DataKinds language extension.

-}

type ASCII'upper superset = ASCII'case 'UpperCase superset

type ASCII'lower superset = ASCII'case 'LowerCase superset
