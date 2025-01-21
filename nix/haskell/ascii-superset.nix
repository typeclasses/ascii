{ mkDerivation, ascii-case, ascii-caseless, ascii-char, base
, bytestring, hashable, hspec, lib, text
}:
mkDerivation {
  pname = "ascii-superset";
  version = "1.3.0.2";
  sha256 = "106c13da5a8d963789f19abb367b0dcae17b0bd260732e6c87307b6fe678cba5";
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
