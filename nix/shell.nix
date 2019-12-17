let
  inherit (import ./default.nix) pkgs haskell ghcid ghcide;
in
  pkgs.mkShell { buildInputs = [ haskell ghcid ghcide ]; }
