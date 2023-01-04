### 1.4.0.0

Additions to the `ASCII` module: `disregardCase`, `ASCII'case`, `ASCII'upper`,
`ASCII'lower`, `KnownCase (..)`, `refineCharToCase`, `refineStringToCase`

Update `ascii-superset` to `1.2.0`. This adds `CharSuperset (toCaseChar)`,
`StringSuperset (toCaseString)`, `refineCharToCase`, and `refineStringToCase`.

### 1.3.1.0 (2023-01-03)

Update `ascii-th` to `1.1.1`.

This adds, most notably, to the `ASCII.QuasiQuoters` module.
The new quasi-quoters are `caseless`, `lower`, and `upper`.
These are also re-exported from the `ASCII` module.

### 1.3.0.0 (2023-01-03)

Update `ascii-superset` to `1.1.0`.

This adds several classes to the `ASCII.Superset` module: `ToChar`, `FromChar`,
`ToString`, `FromString`, `ToCaselessChar`, and `ToCaselessString`.

This is a breaking change because these are superclasses of the existing
`CharSuperset` and `StringSuperset` classes, and they take methods from them.

### 1.2.6.0 (2023-01-02)

Update `ascii-superset` to `1.0.2`. This adds the `ASCII.CaseRefinement` module.

### 1.2.5.0 (2023-01-02)

Add the `ASCII.Caseless` module (re-exported from the `ascii-caseless` package)

Additions to the `ASCII` module:

* `CaselessChar`

### 1.2.4.1 (2022-12-30)

Metadata changes only

### 1.2.4.0 (2022-12-23)

Bump version of `ascii-case` to `1.0.1`. This adds the following function to the
`ASCII.Case` module:

```haskell
opposite :: Case -> Case
```

### 1.2.3.0 (2022-05-04)

Add `isVisible :: Char -> Bool`. Visible characters include all print characters
other than `Space`.

### 1.2.2.0 (2022-04-29)

Add `type UnicodeChar = Data.Char.Char` type alias to `ASCII` module

### 1.2.1.0 (2022-04-29)

New polymorphic narrowing functions:

* `toAsciiCharMaybe :: CharSuperset char => char -> Maybe Char`
* `toDigitMaybe :: DigitSuperset char => char -> Maybe Digit`
* `toHexCharMaybe :: HexCharSuperset char => char -> Maybe HexChar`

New monomorphic character conversion functions:

* `digitToWord8 :: Digit -> Word8`
* `word8ToDigitMaybe :: Word8 -> Maybe Digit`
* `word8ToDigitUnsafe :: Word8 -> Digit`
* `digitToChar :: Digit -> Char`
* `charToDigitMaybe :: Char -> Maybe Digit`
* `charToDigitUnsafe :: Char -> Digit`
* `digitToUnicode :: Digit -> Unicode.Char`
* `unicodeToDigitMaybe :: Unicode.Char -> Maybe Digit`
* `unicodeToDigitUnsafe :: Unicode.Char -> Digit`
* `hexCharToWord8 :: HexChar -> Word8`
* `word8ToHexCharMaybe :: Word8 -> Maybe HexChar`
* `word8ToHexCharUnsafe :: Word8 -> HexChar`
* `hexCharToChar :: HexChar -> Char`
* `charToHexCharMaybe :: Char -> Maybe HexChar`
* `charToHexCharUnsafe :: Char -> HexChar`
* `hexCharToUnicode :: HexChar -> Unicode.Char`
* `unicodeToHexCharMaybe :: Unicode.Char -> Maybe HexChar`
* `unicodeToHexCharUnsafe :: Unicode.Char -> HexChar`

### 1.2.0.0 (2022-04-20)

Update to `ascii-numbers` version `1.1.0`. The major change is that there are
now `Lift` instances for `Digit` and `HexChar`.

### 1.1.3.0

Added functions `digitString` and `hexCharString`

### 1.1.2.0

Add dependency on `ascii-numbers`

New modules:

* `ASCII.Decimal`
* `ASCII.Hexadecimal`

New types:

* `Digit`
* `HexChar`

New classes:

* `DigitSuperset`
* `DigitStringSuperset`
* `HexCharSuperset`
* `HexStringSuperset`

New functions:

* `showIntegralDecimal`
* `showIntegralHexadecimal`
* `readIntegralDecimal`
* `readIntegralHexadecimal`

* `showNaturalDigits`
* `readNaturalDigits`
* `showNaturalHexChars`
* `readNaturalHexChars`

* `showNaturalDecimal`
* `showNaturalHexadecimal`
* `readNaturalDecimal`
* `readNaturalHexadecimal`

Dropped support for old versions:

* Drop support for `base` 4.11 (GHC 8.4)
* Drop support for `base` 4.12 (GHC 8.6)

### 1.1.1.4

Switch test-suite over to `hedgehog`

### 1.1.1.2

Support GHC 9.2

### 1.1.1.0

New functions:

  - `isAlphaNum`
  - `isLetter`
  - `isDigit`
  - `isOctDigit`
  - `isHexDigit`
  - `isSpace`
  - `isPunctuation`
  - `isSymbol`

### 1.1.0.0

The dependency on the 'data-ascii' package is removed, and the following modules
are no longer re-exported:

  - `Data.Ascii`
  - `Data.Ascii.Blaze`
  - `Data.Ascii.ByteString`
  - `Data.Ascii.Word8`

### 1.0.1.6

Add a test suite

Raise `text` lower bound to 1.2.3

### 1.0.1.4

Support GHC 9.0

### 1.0.1.2

Support `bytestring-0.11`

### 1.0.1.0

New functions:

  - `byteStringToUnicodeStringMaybe`
  - `unicodeStringToByteStringMaybe`
  - `byteListToUnicodeStringMaybe`
  - `unicodeStringToByteListMaybe`
  - `convertCharMaybe`
  - `convertCharOrFail`
  - `convertStringMaybe`
  - `convertStringOrFail`

### 1.0.0.2

Support GHC 8.10

### 1.0.0.0

Completely redesigned the library
