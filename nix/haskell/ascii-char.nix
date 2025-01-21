{ mkDerivation, base, hashable, hspec, lib }:
mkDerivation {
  pname = "ascii-char";
  version = "1.0.1.0";
  sha256 = "9b56ef31b90e0ef697e7624c8054e957cf155d3df68a71318766e837b81f9aba";
  revision = "2";
  editedCabalFile = "1x0ci7j3bdlrrza78n53xw4y1dl4py3gqrym0lb6l9w5n7l138gs";
  libraryHaskellDepends = [ base hashable ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/typeclasses/ascii-char";
  description = "A Char type representing an ASCII character";
  license = lib.licenses.asl20;
}
