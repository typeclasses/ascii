rec {

  ghcVersion = "ghc865";

  pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/tarball/7351aa52acd056df3ed472747e3a003f590e5829") {};

  inherit (pkgs) callPackage;

  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  haskell = callPackage ./haskell.nix { inherit haskellPackages; };

  inherit (haskellPackages) ghcid;

  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/bde3f34356b5154750acbba3d06205f03662a72d") {})."ghcide-${ghcVersion}";

}
