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
