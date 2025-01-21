{ mkDerivation, ascii-char, base, hashable, hspec, lib }:
mkDerivation {
  pname = "ascii-case";
  version = "1.0.1.4";
  sha256 = "44856dd718b7a4006e2ac61ac489325bece2b7bd3183986cfa522bdcd8a492fe";
  libraryHaskellDepends = [ ascii-char base hashable ];
  testHaskellDepends = [ ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-case";
  description = "ASCII letter case";
  license = lib.licenses.asl20;
}
