{ mkDerivation, ascii-case, ascii-char, ascii-superset, base
, bytestring, hashable, hedgehog, invert, lib, text
}:
mkDerivation {
  pname = "ascii-numbers";
  version = "1.2.0.0";
  sha256 = "59fc670d7a271c3cebb1850ae2e5d6d304590e8a62f45b5f5510230bf9798214";
  libraryHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable text
  ];
  testHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable
    hedgehog invert text
  ];
  homepage = "https://github.com/typeclasses/ascii-numbers";
  description = "ASCII representations of numbers";
  license = lib.licenses.asl20;
}
