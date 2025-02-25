# Nix flake based on Cardano base
# https://github.com/input-output-hk/cardano-base/blob/master/flake.nix
{
  inputs = {
    haskellNix = { url = "github:input-output-hk/haskell.nix"; };
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
      follows = "haskellNix/nixpkgs-unstable";
    };
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    flake-utils = {
      url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    };
  };
  outputs = { self, nixpkgs, haskellNix, iohkNix, CHaP, flake-utils }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = system:
        let
          overlay = final: prev: {
            haskell-nix = prev.haskell-nix // {
              extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings
                // {
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
        };
    in flake-utils.lib.eachSystem supportedSystems perSystem;

}
