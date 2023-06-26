{ mkDerivation, ascii-char, base, hashable, hedgehog, lib }:
mkDerivation {
  pname = "ascii-group";
  version = "1.0.0.16";
  sha256 = "d8c2dacddf9bbda41bd33ffd9965a33be7cd7985400360ac31e8d07f96291086";
  libraryHaskellDepends = [ ascii-char base hashable ];
  testHaskellDepends = [ ascii-char base hedgehog ];
  homepage = "https://github.com/typeclasses/ascii-group";
  description = "ASCII character groups";
  license = lib.licenses.asl20;
}
