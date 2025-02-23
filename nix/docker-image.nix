{ pkgs, linux-package, version }:
let
  unpack = ''
    mkdir -p $out
    tar -xvf \
      ${linux-package}/cardano-deposit-wallet-${version}-linux64.tar.gz \
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
