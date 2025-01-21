{ mkDerivation, base, containers, criterion, generic-deriving
, hashable, lib, unordered-containers, vector
}:
mkDerivation {
  pname = "invert";
  version = "1.0.0.5";
  sha256 = "0b32de953c5e7bdf5771becd910f415c8b03ff407176fcfe233a36e9441c819e";
  libraryHaskellDepends = [
    base containers generic-deriving hashable unordered-containers
    vector
  ];
  testHaskellDepends = [
    base containers generic-deriving hashable unordered-containers
    vector
  ];
  benchmarkHaskellDepends = [
    base containers criterion generic-deriving hashable
    unordered-containers vector
  ];
  homepage = "https://github.com/typeclasses/invert";
  description = "Automatically generate a function’s inverse";
  license = lib.licenses.asl20;
}
