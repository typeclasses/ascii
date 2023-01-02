{ mkDerivation, ascii-case, ascii-caseless, ascii-char, base
, bytestring, hashable, hspec, lib, text
}:
mkDerivation {
  pname = "ascii-superset";
  version = "1.0.2.0";
  sha256 = "0e238c5005624a14ec12bb31557eb0ee26fc997394722cfcf2b461d585e1a197";
  libraryHaskellDepends = [
    ascii-case ascii-caseless ascii-char base bytestring hashable text
  ];
  testHaskellDepends = [
    ascii-case ascii-caseless ascii-char base hspec text
  ];
  homepage = "https://github.com/typeclasses/ascii-superset";
  description = "Representing ASCII with refined supersets";
  license = lib.licenses.asl20;
}
