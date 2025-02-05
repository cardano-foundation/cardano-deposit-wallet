{
  description = "Cardano Deposit Wallet";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    wallet.url =
      "github:cardano-foundation/cardano-wallet?rev=13a411f9ed2b05432d528343deb18b171fc1a424";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, wallet, ... }:

    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        stdenv = pkgs.stdenv;
        buildPlatform = stdenv.buildPlatform;
        version = "0.0.1";
        mkPackage = p:
          pkgs.callPackage ./nix/package.nix (p // {
            inherit stdenv;
            inherit version;
          });
        wallet-artifact = wallet.packages.${system}.ci.artifacts;
        onAttrs = pkgs.lib.optionalAttrs;
      in onAttrs buildPlatform.isLinux {
        packages.linux.package = mkPackage {
          wallet-package = wallet-artifact.linux64.release;
          platform = "linux64";
        };
      } // onAttrs buildPlatform.isMacOS {
        packages.macos-silicon = onAttrs buildPlatform.isAarch64 {
          package = mkPackage {
            wallet-package = wallet-artifact.macos-silicon.release;
            platform = "macos-silicon";
          };
        };
        packages.macos-intel = onAttrs buildPlatform.isx86_64 {
          package = mkPackage {
            wallet-package = wallet-artifact.macos-intel.release;
            platform = "macos-intel";
          };
        };
      } // {
        devShells.default = pkgs.mkShell {
          packages = [
            wallet.packages.${system}.cardano-wallet
            wallet.inputs.cardano-node-runtime.${system}.cardano-node
            wallet.inputs.cardano-node-runtime.${system}.cardano-cli
            wallet.packages.${system}.cardano-address
            wallet.packages.${system}.bech32
          ];
          shellHook = ''
            echo "********* Deposit wallet shell *********"
            echo "comes with cardano-wallet, cardano-node, cardano-cli, cardano-address and bech32"
            echo "****************************************"

          '';
        };
      }

    );
}
