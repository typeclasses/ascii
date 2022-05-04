module ASCII.ListsAndPredicates
  (
    {- * Re-exported modules -} module ASCII.Lists, module ASCII.Predicates
    {- * Notes -} {- $notes -}
  )
  where

import ASCII.Lists
import ASCII.Predicates

{- $notes

The two modules re-exported here present the same information in two different forms:

  - As a list of all ASCII characters with some classification;
  - As a function that tests whether a particular character belongs to the classification.

Each list contains exactly the characters for which its corresponding predicate is true.

>>> controlCodes == filter isControl all
True

>>> printableCharacters == filter isPrint all
True

>>> letters == filter isLetter all
True

>>> capitalLetters == filter isUpper all
True

>>> smallLetters == filter isLower all
True

>>> digits == filter isDigit all
True

>>> numbers == filter isNumber all
True

>>> octDigits == filter isOctDigit all
True

>>> hexDigits == filter isHexDigit all
True

>>> visibleCharacters == filter isVisible all
True

-}
