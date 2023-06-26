{ mkDerivation, ascii-case, ascii-char, base, hashable, hspec, lib
}:
mkDerivation {
  pname = "ascii-caseless";
  version = "0.0.0.1";
  sha256 = "9865b552a66d782a08e2a86ada3ef3520e6c1008ff8914b335bf453cc6100b2d";
  libraryHaskellDepends = [ ascii-case ascii-char base hashable ];
  testHaskellDepends = [ ascii-case ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-caseless";
  description = "ASCII character without an upper/lower case distinction";
  license = lib.licenses.asl20;
}
