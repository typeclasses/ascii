ASCII
=====

What is ASCII?
--------------

The *American Standard Code for Information Interchange* (ASCII) comprises a set
of 128 characters, each represented by 7 bits. 33 of these characters are
"control codes"; a few of these are still in use, but most are obsolete relics
of the early days of computing. The other 95 are "printable characters" such as
letters and numbers, mostly corresponding to the keys on an American English
keyboard.

Nowadays instead of ASCII we typically work with text using an encoding such as
UTF-8 that can represent the entire Unicode character set, which includes over a
hundred thousand characters and is not limited to the symbols of any particular
writing system or culture. However, ASCII is still relevant to network
protocols; for example, we can see it in the specification of [HTTP message
headers][ietf].

There is a convenient relationship between ASCII and Unicode: the ASCII
characters are the first 128 characters of the much larger Unicode character
set. The [C0 Controls and Basic Latin][unicode] section of the Unicode standard
contains a list of all the ASCII characters.

Haskell packages
----------------

This repository contains several packages.

  * The main API is the [`ASCII`][ascii] module in the `ascii` package, which
    is an amalgamation of the other smaller packages.

  * If you only need the ASCII [`Char`][char] type, you can use the
    `ascii-char` package, which is minimal so that it can be kept stable.

  * The `ascii-group` package defines the [`Group`][group] type (`Control` and
    `Printable`), and the `ascii-case` package defines the [`Case`][case] type
    (`UpperCase` and `LowerCase`). These package are also small and stable.

  * The `ascii-predicates` package provides [additional ways of categorizing
    characters][predicates] similar to what you can find in [the `base`
    package][base].

  * The `ascii-superset` package defines [`CharSuperset` and
    `StringSuperset`][superset] classes to generalize types that represent
    characters and strings, respectively, in character sets larger than ASCII.
    It also defines the [`ASCII`][refinement] type constructor, which is used
    to indicate that a value from some ASCII superset is confined to ASCII.

  * The `ascii-numbers` package provides utilities for working with numbers
    represented using ASCII digits 0-9, ASCII letters A-F to represent
    hexadecimal digits 10-15, and the `HypenMinus` character for negation.

  * The `ascii-th` package provides a [quasi-quoter][qq] that allows one to
    safely and conveniently express ASCII string literals. The generated
    expressions are polymorphic and can take the form of any type belonging to
    the `StringSuperset` class, including `[ASCII.Char]`, [`String`][string],
    [`ByteString`][bytestring], and [`Text`][text].

  [ietf]: https://tools.ietf.org/html/rfc7230#section-1.2
  [unicode]: https://www.unicode.org/charts/PDF/U0000.pdf
  [ascii]: https://hackage.haskell.org/package/ascii/docs/ASCII.html
  [char]: https://hackage.haskell.org/package/ascii-char/docs/ASCII-Char.html
  [group]: https://hackage.haskell.org/package/ascii-group/docs/ASCII-Group.html
  [case]: https://hackage.haskell.org/package/ascii-case/docs/ASCII-Case.html
  [predicates]: https://hackage.haskell.org/package/ascii-predicates/docs/ASCII-ListsAndPredicates.html
  [base]: https://hackage.haskell.org/package/base/docs/Data-Char.html
  [superset]: https://hackage.haskell.org/package/ascii-superset/docs/ASCII-Superset.html
  [refinement]: https://hackage.haskell.org/package/ascii-superset/docs/ASCII-Refinement.html
  [qq]: https://hackage.haskell.org/package/ascii-th/docs/ASCII-QuasiQuoters.html
  [string]: https://hackage.haskell.org/package/base/docs/Data-String.html
  [bytestring]: https://hackage.haskell.org/package/bytestring
  [text]: https://hackage.haskell.org/package/text
