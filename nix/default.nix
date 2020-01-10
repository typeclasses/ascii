rec {

  ghcVersion = "ghc865";

  pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/tarball/e1eedf29e5d22e6824e614d75449b75a2e3455d6") {};

  inherit (pkgs) callPackage;

  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  haskell = callPackage ./haskell.nix { inherit haskellPackages; };

  inherit (haskellPackages) ghcid;

  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/f0de6033203963f1b40e2ee38fb60d36873449ac") {})."ghcide-${ghcVersion}";

}
