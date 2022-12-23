








-- todo: tests for this module!













module ASCII.CaseRefinement
  (
    {- * ASCII'case type constructor -} ASCII'case, lift, asciiCaseUnsafe,
    {- ** Aliases -} {- $aliases -} ASCII'upper, ASCII'lower,
    {- * Character functions -} validateChar, fromCaselessChar, toCaselessChar, substituteChar, asCaselessChar,
    {- * String functions -} validateString, fromCaselessCharList, toCaselessCharList, substituteString, mapChars,
  )
  where

import ASCII.Case (Case (..))
import ASCII.Caseless (CaselessChar)
import ASCII.Superset (CharSuperset, StringSuperset)

import qualified ASCII.Char as ASCII
import qualified ASCII.Case as Case
import qualified ASCII.Caseless as Caseless
import qualified ASCII.Superset as Superset

import Control.Monad (guard)
import Data.Bool (Bool (..))
import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Foldable (any)
import Data.Function (id, ($), (.))
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>))
import Data.Semigroup (Semigroup)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Prelude (succ)
import Text.Show (Show, showList, showParen, showString, showsPrec)

import qualified Data.Bool as Bool
import qualified Data.List as List

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

instance Show superset => Show (ASCII'case letterCase superset)
  where
    showsPrec d x = showParen (d > app_prec) $
        showString "asciiCaseUnsafe " . showsPrec (succ app_prec) (lift x)
      where app_prec = 10

    showList x = showString "asciiCaseUnsafe " . showList (List.map lift x)

asciiCaseUnsafe :: superset -> ASCII'case letterCase superset
asciiCaseUnsafe = ASCII'case_Unsafe

---

{- $aliases

The 'ASCII'upper' and 'ASCII'lower' type aliases exist primarily so that you can use 'ASCII'case' without the DataKinds language extension.

-}

type ASCII'upper superset = ASCII'case 'UpperCase superset

type ASCII'lower superset = ASCII'case 'LowerCase superset

---

class KnownCase (letterCase :: Case) where
    theCase :: Case

oppositeCase :: Case -> Case
oppositeCase UpperCase = LowerCase
oppositeCase LowerCase = UpperCase

---

-- | Return 'Just' an 'ASCII'case' character if the input is an ASCII character in the proper case, or 'Nothing' otherwise
validateChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    superset -> Maybe (ASCII'case letterCase superset)
validateChar x = do
    c <- Superset.toCharMaybe x
    guard (Bool.not (Case.isCase (oppositeCase (theCase @letterCase)) c))
    Just (asciiCaseUnsafe x)

-- | Return an 'ASCII'case' character if the input is an ASCII character in the proper case, or 'ASCII.Substitute' otherwise
substituteChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    superset -> ASCII'case letterCase superset
substituteChar x = case validateChar x of
    Nothing -> asciiCaseUnsafe (Superset.fromChar ASCII.Substitute)
    Just c -> c

-- | Lift a 'CaselessChar' into a superset type, wrapped in the 'ASCII'case' refinement to save the evidence that it is ASCII in a particular case
fromCaselessChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    CaselessChar -> ASCII'case letterCase superset
fromCaselessChar = asciiCaseUnsafe . Superset.fromChar . Caseless.toCase (theCase @letterCase)

-- | Given a character from some type that is known to represent an ASCII character in a particular case, obtain the caseless ASCII character it represents
toCaselessChar :: CharSuperset superset =>
    ASCII'case letterCase superset -> CaselessChar
toCaselessChar =  Caseless.disregardCase . Superset.toCharUnsafe . lift

-- | Given a character from a larger set that is known to represent an ASCII character, manipulate it as if it were an ASCII character
asCaselessChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    (CaselessChar -> CaselessChar) -> ASCII'case letterCase superset -> ASCII'case letterCase superset
asCaselessChar f = asciiCaseUnsafe . Superset.asCharUnsafe g . lift
  where
    g = Caseless.toCase (theCase @letterCase) . f . Caseless.assumeCaseUnsafe (theCase @letterCase)

---

-- | Return 'Just' an 'ASCII'case' string if the input consists entirely of ASCII characters in the proper case, or 'Nothing' otherwise
validateString :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    superset -> Maybe (ASCII'case letterCase superset)
validateString x = do
    s <- Superset.toCharListMaybe x
    guard (Bool.not (any (Case.isCase (oppositeCase (theCase @letterCase))) s))
    Just (asciiCaseUnsafe x)

-- | Lift a list of 'CaselessChar' into a superset string type, wrapped in the 'ASCII'case' refinement to save the evidence that all of the characters in the string are ASCII in a particular case.
fromCaselessCharList :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    [CaselessChar] -> ASCII'case letterCase superset
fromCaselessCharList = asciiCaseUnsafe . Superset.fromCharList . List.map (Caseless.toCase (theCase @letterCase))

-- | Given a string from some type that is known to represent only ASCII characters in a particular case, obtain the caseless characters it represents
toCaselessCharList :: forall letterCase superset. KnownCase letterCase => StringSuperset superset => ASCII'case letterCase superset -> [CaselessChar]
toCaselessCharList = List.map (Caseless.assumeCaseUnsafe (theCase @letterCase)) . Superset.toCharListUnsafe . lift

-- | Forces a string from a larger character set into cased ASCII by using the 'ASCII.Substitute' character in place of any unacceptable characters.
substituteString :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    superset -> ASCII'case letterCase superset
substituteString = asciiCaseUnsafe . Superset.fromCharList . List.map f . Superset.toCharListSub
  where
    f x = if Case.isCase (oppositeCase (theCase @letterCase)) x
          then ASCII.Substitute
          else x

-- | Given a string from a larger set that is known to consist entirely of ASCII characters in a particular case, map over the characters in the string as if they were caseless ASCII characters
mapChars :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    (CaselessChar -> CaselessChar) -> ASCII'case letterCase superset -> ASCII'case letterCase superset
mapChars f = asciiCaseUnsafe . Superset.mapCharsUnsafe g . lift
  where
    g = Caseless.toCase (theCase @letterCase) . f . Caseless.assumeCaseUnsafe (theCase @letterCase)
