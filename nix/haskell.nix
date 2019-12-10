{ haskellPackages }:

haskellPackages.ghcWithPackages (p: [
    p.bytestring
    p.generic-deriving
    p.memory
    p.network
    p.template-haskell
    p.text
])
