{ mkDerivation, ascii-char, base, hedgehog, lib }:
mkDerivation {
  pname = "ascii-predicates";
  version = "1.0.1.3";
  sha256 = "ae4ed704ea77fbff9c8fb107d6b35413a121f3674c63156236af776639009ebd";
  libraryHaskellDepends = [ ascii-char base ];
  testHaskellDepends = [ ascii-char base hedgehog ];
  homepage = "https://github.com/typeclasses/ascii-predicates";
  description = "Various categorizations of ASCII characters";
  license = lib.licenses.asl20;
}
