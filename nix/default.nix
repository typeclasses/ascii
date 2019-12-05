rec {

  pkgs = import <nixpkgs> {};

  inherit (pkgs) callPackage haskellPackages;

  haskell = callPackage ./haskell.nix {};

  inherit (haskellPackages) ghcid;

}
