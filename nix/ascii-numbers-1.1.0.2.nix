{ mkDerivation, ascii-case, ascii-char, ascii-superset, base
, bytestring, hashable, hedgehog, invert, lib, text
}:
mkDerivation {
  pname = "ascii-numbers";
  version = "1.1.0.2";
  sha256 = "dbfe4acb0eaa2f0c4ce68c6ac6072297cc485b5c7f9b3ad59375c36133b61837";
  revision = "1";
  editedCabalFile = "1g3aazd8jdadzhf9dp3p1x5m3bdvng0zmg4madkg9750qh0vdspr";
  libraryHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable text
  ];
  testHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable
    hedgehog invert text
  ];
  homepage = "https://github.com/typeclasses/ascii-numbers";
  description = "ASCII representations of numbers";
  license = lib.licenses.asl20;
}
