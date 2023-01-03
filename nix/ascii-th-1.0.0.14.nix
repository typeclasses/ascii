{ mkDerivation, ascii-char, ascii-superset, base, bytestring, hspec
, lib, template-haskell, text
}:
mkDerivation {
  pname = "ascii-th";
  version = "1.0.0.14";
  sha256 = "300d9439b053304e1d9e5d287a2a25e83fceea62f35d16346ed7af93f9b1a072";
  revision = "1";
  editedCabalFile = "0ddljiw2mqp3ypz19kkly4xwqs4qsk6xi14n3qr8vnxxsv91rign";
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
