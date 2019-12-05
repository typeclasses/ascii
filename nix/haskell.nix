{ haskellPackages }:

haskellPackages.ghcWithPackages (p: [
    p.bytestring
    p.generic-deriving
    p.memory
    p.template-haskell
])
