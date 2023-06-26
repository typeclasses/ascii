{ mkDerivation, ascii-char, base, hashable, hspec, lib }:
mkDerivation {
  pname = "ascii-case";
  version = "1.0.1.3";
  sha256 = "1c4db1b2510934b78c2adc0ef6e52701d7f31ccc11162df7982879d25c440c19";
  libraryHaskellDepends = [ ascii-char base hashable ];
  testHaskellDepends = [ ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-case";
  description = "ASCII letter case";
  license = lib.licenses.asl20;
}
