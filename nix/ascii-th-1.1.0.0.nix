{ mkDerivation, ascii-char, ascii-superset, base, bytestring, hspec
, lib, template-haskell, text
}:
mkDerivation {
  pname = "ascii-th";
  version = "1.1.0.0";
  sha256 = "983f8d95a36e83a33f15e96890ad78ccdd06a33858b2a33a686e80a3d6d52828";
  libraryHaskellDepends = [
    ascii-char ascii-superset base template-haskell
  ];
  testHaskellDepends = [
    ascii-char ascii-superset base bytestring hspec text
  ];
  homepage = "https://github.com/typeclasses/ascii-th";
  description = "Template Haskell support for ASCII";
  license = lib.licenses.asl20;
}
