{ pkgs, wallet-package, version, platform, ... }:

let
  unpack = ''
    mkdir -p $out
    tar -xvf \
      ${wallet-package}/cardano-deposit-wallet-${version}-${platform}.tar.gz \
      -C $out
  '';
in pkgs.dockerTools.buildImage {
  name = "cardanofoundation/cardano-deposit-wallet";
  tag = version;
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = let
      unpacked = pkgs.runCommand "unpack" {
        nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
      } unpack;
    in [ unpacked ];
  };
}
