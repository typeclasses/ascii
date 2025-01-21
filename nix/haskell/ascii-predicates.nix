{ mkDerivation, ascii-char, base, hedgehog, lib }:
mkDerivation {
  pname = "ascii-predicates";
  version = "1.0.1.4";
  sha256 = "1a198a9a5505691560450bba9326e91284c7c48a31fd07442a5b282b3a85dfff";
  libraryHaskellDepends = [ ascii-char base ];
  testHaskellDepends = [ ascii-char base hedgehog ];
  homepage = "https://github.com/typeclasses/ascii-predicates";
  description = "Various categorizations of ASCII characters";
  license = lib.licenses.asl20;
}
