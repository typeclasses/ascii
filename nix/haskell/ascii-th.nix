{ mkDerivation, ascii-case, ascii-caseless, ascii-char
, ascii-superset, base, bytestring, hspec, lib, template-haskell
, text
}:
mkDerivation {
  pname = "ascii-th";
  version = "1.2.0.1";
  sha256 = "09b9da4fbb9ce3a600c00390b214964a6c4c260522a59a69c346350adc53473e";
  libraryHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-superset base
    template-haskell
  ];
  testHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-superset base bytestring
    hspec text
  ];
  homepage = "https://github.com/typeclasses/ascii-th";
  description = "Template Haskell support for ASCII";
  license = lib.licenses.asl20;
}
