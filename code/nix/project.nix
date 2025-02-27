{ CHaP, system, indexState, src, haskell-nix, ... }:
let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.gitAndTools.git
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
    ];
    shellHook = ''
      echo "Entering shell for cardano-deposit-wallet development"
    '';
  };
  fullyStaticOptions = { pkgs, ... }:
    let libs = with pkgs; [ zlib openssl libffi gmp6 pkgs.secp256k1 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") (libs);
    };
  musl = { pkgs, ... }: {
    packages.cardano-deposit-wallet-exe.components.exes.cardano-deposit-wallet =
      (fullyStaticOptions { inherit pkgs; });
    doHaddock = false;
  };
  releaseFlags = { lib, ... }: {
    packages = lib.genAttrs [ "cardano-deposit-wallet" ]
      (name: { flags.release = true; });
  };
  libOverlay = { lib, pkgs, ... }: {
    # Use our forked libsodium from iohk-nix crypto overlay.
    packages.plutus-tx.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.byron-spec-ledger.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  };
  postInstall = { lib, pkgs, ... }: { reinstallableLibGhc = true; };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-deposit-wallet";
    compiler-nix-name = "ghc966";
    inherit src;
    shell = shell { inherit pkgs; };
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
    modules = [ releaseFlags libOverlay postInstall ]
      ++ lib.optional pkgs.stdenv.hostPlatform.isMusl musl;
  };
  project = haskell-nix.cabalProject' mkProject;
  packages = {
    inherit project;
    cardano-deposit-wallet =
      project.hsPkgs.cardano-deposit-wallet-exe.components.exes.cardano-deposit-wallet;
    cardano-deposit-wallet-linux64-static =
      project.projectCross.musl64.hsPkgs.cardano-deposit-wallet-exe.components.exes.cardano-deposit-wallet;
  };
in {
  inherit packages;
  devShell = project.shell;
}
