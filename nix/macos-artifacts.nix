# ##############################################################################
# Release package
#
# This bundles up the build of the given exes, with their
# dependencies, and sets up the Hydra build artifact.
#
###############################################################################

{ pkgs, address, code, node, version, configs, rewrite-libs ? null, system, ... }:

let
  inherit (pkgs) lib;
  cardano-node = node.${system}.pkgs.cardano-node;
  cardano-cli = node.${system}.pkgs.cardano-cli;
  cardano-address =
    code.packages.project.hsPkgs.cardano-addresses-cli.components.exes.cardano-address;
  bech32 =
    code.packages.project.hsPkgs.bech32.components.exes.bech32;
  deposit-wallet = code.packages.cardano-deposit-wallet-dynamic;
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
    ''
    + lib.optionalString (rewrite-libs != null) ''
      ( cd $out/unpacked ;
        ${rewrite-libs}/bin/rewrite-libs . `ls -1 | grep -Fv .dylib`
        for a in *; do /usr/bin/codesign -f -s - $a; done
      )
    ''
    + ''
      mkdir -p $out/unpacked/configs
      cp -R ${configs}/cardano/* $out/unpacked/configs
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-macos-silicon.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in {
  packages.macos.package = package-derivation;
  packages.code = code; # useful to access the haskell libraries
  packages.version = version; # useful for CI to tag artifacts
}
# # Rewrite library paths to standard non-nix locations
# ( cd $name; rewrite-libs . `ls -1 | grep -Fv .dylib`
#   for a in *; do /usr/bin/codesign -f -s - $a; done
# )

#   '' + ''
#     # Add configuration files
#     mkdir -p $name/configs
#     cp  --recursive ${nodeConfigs}/cardano/* $name/configs
#     chmod -R +w $name

#   '' + lib.optionalString (isLinux || isMacOS) ''
#     mkdir -p $name/auto-completion/{bash,zsh,fish}
#     cp ${exe}/share/bash-completion/completions/* $name/auto-completion/bash/$exeName.sh
#     cp ${exe}/share/zsh/vendor-completions/* $name/auto-completion/zsh/_$exeName
#     cp ${exe}/share/fish/vendor_completions.d/* $name/auto-completion/fish/$exeName.fish

#   '' + lib.optionalString makeTarball ''
#     tar -czf $out/$pkgname $name
#   '' + lib.optionalString makeZip ''
#     ( cd $name; zip -r $out/$pkgname . )
#   '' + ''
#     echo "file binary-dist $out/$pkgname" > $out/nix-support/hydra-build-products
#   '' + lib.optionalString isWindows ''

#     # make a separate configuration package if needed
#     if [ -d ${exe}/configuration ]; then
#       cp --no-preserve=mode,timestamps -R ${exe}/configuration .

#       ( cd configuration; zip -r $out/$name-configuration.zip . )
#       echo "file binary-dist $out/$name-configuration.zip" >> $out/nix-support/hydra-build-products
#     fi

#     # make a separate deployments configuration package if needed
#     if [ -d ${exe}/deployments ]; then
#       cp --no-preserve=mode,timestamps -R ${exe}/deployments .

#       ( cd deployments; zip -r $out/$name-deployments.zip . )
#       echo "file binary-dist $out/$name-deployments.zip" >> $out/nix-support/hydra-build-products
#     fi
#   '';

#   # test that executables work
#   exeRunner = lib.optionalString isWindows "wine64";
#   checkPhase = ''
#     cd `mktemp -d`
#     echo " - extracting $pkgname"
#     ${lib.optionalString makeTarball "tar -xzvf $out/$pkgname"}
#     ${lib.optionalString makeZip "unzip $out/$pkgname"}

#   '' + lib.optionalString isWindows ''
#     # setup wine
#     export WINEPREFIX=$TMP
#     export HOME=$TMP
#     export WINEDLLOVERRIDES="winemac.drv=d"
#     export WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag

#   '' + ''
#     export PATH=`pwd`/$name:$PATH

#     echo " - running checks"
#     ruby ${../scripts/check-bundle.rb} $exeName $exeRunner
#   '';
# } // lib.optionalAttrs (pkgs.stdenv.buildPlatform.libc == "glibc") {
#   LOCALE_ARCHIVE =
#     "${pkgs.buildPackages.glibcLocales}/lib/locale/locale-archive";
#   LANG = "en_US.UTF-8";
#   LC_ALL = "en_US.UTF-8";
# }
