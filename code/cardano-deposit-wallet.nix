{ system, nixpkgs, haskellNix, iohkNix, CHaP, flake-utils }:
let
  overlay = final: prev: {
    haskell-nix = prev.haskell-nix // {
      extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
        # String pkgconfig-depends names are mapped to lists of Nixpkgs
        # package names
        "libblst" = [ "blst" ];
      };
    };
  };
  pkgs = import nixpkgs {
    overlays = [
      iohkNix.overlays.crypto
      iohkNix.overlays.cardano-lib
      haskellNix.overlay
      iohkNix.overlays.haskell-nix-extra
      overlay
    ];
    inherit system;
  };
  src = ./.;
  indexState = "2024-08-20T21:35:22Z";
in import ./nix/project.nix {
  inherit system;
  inherit indexState;
  inherit CHaP;
  inherit src;
  inherit (pkgs) haskell-nix;
}
