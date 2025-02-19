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
          pkgs = import nixpkgs {
            overlays = [ haskellNix.overlay ]
              ++ builtins.attrValues iohkNix.overlays;
            inherit system;
            inherit (haskellNix) config;
          };
          src = ./.;
          indexState = "2025-02-19T00:00:00Z";
        in import ./nix/project.nix {
          inherit pkgs;
          inherit system;
          inherit indexState;
          inherit CHaP;
          inherit src;
        };
    in flake-utils.lib.eachSystem supportedSystems perSystem;

}
