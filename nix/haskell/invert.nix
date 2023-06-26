{ mkDerivation, base, containers, criterion, generic-deriving
, hashable, lib, unordered-containers, vector
}:
mkDerivation {
  pname = "invert";
  version = "1.0.0.4";
  sha256 = "f4bf304a381d8ddea0553b4d88b83b99cecf66d624a9f7f6c4ad3cc138a936c6";
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
