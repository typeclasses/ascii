{ mkDerivation, ascii-case, ascii-char, base, hashable, hspec, lib
}:
mkDerivation {
  pname = "ascii-caseless";
  version = "0.0.0.2";
  sha256 = "b91d2f4dfc3e7d199374980f2ddeee56b547e6592c33d785afb517bcd6573ddc";
  libraryHaskellDepends = [ ascii-case ascii-char base hashable ];
  testHaskellDepends = [ ascii-case ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-caseless";
  description = "ASCII character without an upper/lower case distinction";
  license = lib.licenses.asl20;
}
