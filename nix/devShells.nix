{ code, node, pkgs, system, ... }:
let hsPkgs = code.packages.project.hsPkgs;
in {
  devShells.default = pkgs.mkShell {
    packages = [
      hsPkgs.cardano-addresses-cli.components.exes.cardano-address
      hsPkgs.bech32.components.exes.bech32
      code.packages.cardano-deposit-wallet-static
      node.${system}.pkgs.cardano-node
      node.${system}.pkgs.cardano-cli
    ];
    shellHook = ''
      echo "********* Deposit wallet shell *********************************************************"
      echo "comes with cardano-deposit-wallet, cardano-node, cardano-cli, cardano-address and bech32"
      echo "****************************************************************************************"
    '';
  };
}
