{ pkgs, address, code, node, version, configs, ... }:
let
  cardano-node = node.x86_64-linux.projectCross.musl64.pkgs.cardano-node;
  cardano-cli = node.x86_64-linux.projectCross.musl64.pkgs.cardano-cli;
  deposit-wallet = code.packages.cardano-deposit-wallet-linux64-static;
  cardano-address =
    code.packages.project.projectCross.musl64.hsPkgs.cardano-addresses-cli.components.exes.cardano-address;
  bech32 =
    code.packages.project.projectCross.musl64.hsPkgs.bech32.components.exes.bech32;
  package-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "cardano-deposit-wallet";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${deposit-wallet}/bin/cardano-deposit-wallet $out/unpacked
      cp ${cardano-node}/bin/cardano-node $out/unpacked
      cp ${cardano-cli}/bin/cardano-cli $out/unpacked
      cp ${cardano-address}/bin/cardano-address $out/unpacked
      cp ${bech32}/bin/bech32 $out/unpacked
      mkdir -p $out/unpacked/configs
      cp -R ${configs}/cardano/* $out/unpacked/configs
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in rec {
  packages.linux64.package = package-derivation;
  packages.code = code; # useful to access the haskell libraries
  packages.cardano-deposit-wallet = code.packages.cardano-deposit-wallet;
  packages.default = packages.cardano-deposit-wallet;
  packages.version = version; # useful for CI to tag artifacts
  packages.docker-image = import ./docker-image.nix {
    linux-package = packages.linux64.package;
    inherit version;
    inherit pkgs;
  };
}
