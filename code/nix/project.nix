{ CHaP, pkgs, system, indexState, src, ... }:
let
  stdenv = pkgs.stdenv;
  lib = pkgs.lib;
  shell = {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.gitAndTools.git
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.stylish-haskell
    ];
    shellHook = "";
  };
  fullyStaticOptions =
    let libs = with pkgs; [ zlib openssl libffi gmp6 secp256k1 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") libs;
    };
  musl = lib.optionalAttrs stdenv.hostPlatform.isMusl {
    packages.cardano-deposit-wallet-exe-transition.components.exes.cardano-deposit-wallet =
      fullyStaticOptions;
    doHaddock = false;
  };
  releaseFlags = {
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
  project = pkgs.haskell-nix.cabalProject' rec {
    name = "cardano-deposit-wallet-transition";
    compiler-nix-name = "ghc966";
    inherit src;
    inherit shell;
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
    modules = [ releaseFlags musl libOverlay ];
  };
  crossCompilation = pkgs.lib.optionalAttrs (system == "x86_64-linux") {
    crossPlatforms = p: [ p.mingwW64 p.musl64 ];
  };
in project.flake crossCompilation
