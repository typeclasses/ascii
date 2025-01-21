{ mkDerivation, ascii-char, base, hashable, hspec, lib }:
mkDerivation {
  pname = "ascii-group";
  version = "1.0.0.17";
  sha256 = "ab4b0ba9ea3b68edf5412a3b3252530b4b77e66e35bdd2cffd11a84ec1efc2b2";
  libraryHaskellDepends = [ ascii-char base hashable ];
  testHaskellDepends = [ ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-group";
  description = "ASCII character groups";
  license = lib.licenses.asl20;
}
