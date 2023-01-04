{ mkDerivation, ascii-case, ascii-caseless, ascii-char
, ascii-superset, base, bytestring, hspec, lib, template-haskell
, text
}:
mkDerivation {
  pname = "ascii-th";
  version = "1.1.1.0";
  sha256 = "e595a352558f5a0bd34062ea9217d89f4c7959f521acb6b55a6b811cf391d2c9";
  revision = "1";
  editedCabalFile = "06dsa4nrpvy1sm4hr4q6ydgjizf4r7s9xvlc9ra4f8mawsq85zx6";
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
