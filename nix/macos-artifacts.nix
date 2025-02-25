# ##############################################################################
# Release package
#
# This bundles up the build of the given exes, with their
# dependencies, and sets up the Hydra build artifact.
#
###############################################################################

{ pkgs, address, code, node, version, configs, rewrite-libs ? null, system, ...
}:

let
  inherit (pkgs) lib;
  cardano-node = node.${system}.pkgs.cardano-node;
  cardano-cli = node.${system}.pkgs.cardano-cli;
  cardano-address =
    code.packages.project.hsPkgs.cardano-addresses-cli.components.exes.cardano-address;
  bech32 = code.packages.project.hsPkgs.bech32.components.exes.bech32;
  deposit-wallet = code.packages.cardano-deposit-wallet;
  package-derivation = pkgs.stdenv.mkDerivation {
    pname = "cardano-deposit-wallet";
    inherit version;
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [ "unpackPhase" "installPhase" ];
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp -R ${deposit-wallet}/bin/* $out/unpacked
      cp ${cardano-node}/bin/cardano-node $out/unpacked
      cp ${cardano-cli}/bin/cardano-cli $out/unpacked
      cp ${cardano-address}/bin/cardano-address $out/unpacked
      cp ${bech32}/bin/bech32 $out/unpacked
    '' + lib.optionalString (rewrite-libs != null) ''
      ( cd $out/unpacked ;
        ${rewrite-libs}/bin/rewrite-libs . `ls -1 | grep -Fv .dylib`
        for a in *; do /usr/bin/codesign -f -s - $a; done
      )
    '' + ''
      mkdir -p $out/unpacked/configs
      cp -R ${configs}/cardano/* $out/unpacked/configs
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-macos-silicon.tar.gz .
      rm -rf $out/unpacked
    '';
  };
  common = rec {
    packages = {
      code = code; # useful to access the haskell libraries
      version = version; # useful for CI to tag artifacts
      cardano-deposit-wallet = code.packages.cardano-deposit-wallet;
    };
    default = code.packages.cardano-deposit-wallet;
  };
  intel = lib.optionalAttrs (system == "x86_64-darwin") rec {
    packages.macos-intel.package = package-derivation;
  };
  silicon = lib.optionalAttrs (system == "aarch64-darwin") rec {
    packages.macos-silicon.package = package-derivation;
  };
in lib.recursiveUpdate common (intel // silicon)
