{ code, node, pkgs, system, ... }:
let hsPkgs = code.packages.project.hsPkgs;
in {
  devShells.default = pkgs.mkShell {
    packages = [
      hsPkgs.cardano-addresses-cli.components.exes.cardano-address
      hsPkgs.bech32.components.exes.bech32
      code.packages.cardano-deposit-wallet
      node.${system}.pkgs.cardano-node
      node.${system}.pkgs.cardano-cli
    ];
    shellHook = ''
      echo "********* Deposit wallet shell **********************************************************"
      echo " Comes with cardano-deposit-wallet, cardano-node, cardano-cli, cardano-address and bech32"
      echo " If you need to hack with the deposit wallet, use the `./code` directory flake"
      echo "*****************************************************************************************"
    '';
  };
}
