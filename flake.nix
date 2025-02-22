{
  description = "Cardano Deposit Wallet";

  inputs = {
    # flake-utils.url = "github:numtide/flake-utils";
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
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    };
    cardano-address = {
      url = "github:intersectMBO/cardano-addresses?ref=4.0.0";
    };
    bech32 = { url = "github:intersectMBO/bech32"; };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, cardano-address
    , cardano-node-runtime, ... }:
    let
      version = self.dirtyShortRev or self.shortRev;
      node = cardano-node-runtime.project;
      address = cardano-address.packages;
      configs = ./configs;
      perSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };
          buildPlatform = pkgs.stdenv.buildPlatform;
          onAttrs = pkgs.lib.optionalAttrs;
          onLinux = onAttrs buildPlatform.isLinux;
          onMacOS = onAttrs buildPlatform.isMacOS;
          code = import ./code/cardano-deposit-wallet.nix {
            inherit system;
            inherit (inputs) nixpkgs haskellNix iohkNix CHaP flake-utils;
          };
          devShells =
            import ./nix/devShells.nix { inherit code node pkgs system; };
          linux-artifacts = import ./nix/linux-artifacts.nix {
            inherit pkgs address code node version configs;
          };
          macos-artifacts = { packages = { }; };
        in {
          inherit (devShells) devShells;
        } // onLinux { inherit (linux-artifacts) packages; }
        // onMacOS { inherit (macos-artifacts) packages; };
    in flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ] perSystem;
}
