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
