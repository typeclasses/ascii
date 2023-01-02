{ mkDerivation, ascii-char, base, hedgehog, lib }:
mkDerivation {
  pname = "ascii-predicates";
  version = "1.0.1.1";
  sha256 = "aceeb9d3c673e519f1bd8574314539cfa0e7760eeb5c8e9230a4c9136e6913e5";
  libraryHaskellDepends = [ ascii-char base ];
  testHaskellDepends = [ ascii-char base hedgehog ];
  homepage = "https://github.com/typeclasses/ascii-predicates";
  description = "Various categorizations of ASCII characters";
  license = lib.licenses.asl20;
}
