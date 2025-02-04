{ stdenv, wallet-package, platform, version }:

stdenv.mkDerivation rec {
  pname = "cardano-deposit-wallet";
  inherit version;
  src = [ wallet-package ];
  installPhase = ''
    NAME=$(ls . | head -n 1)
    tar -xvf $NAME --strip-components=1
    rm $NAME
    tar -czvf $pname-$version-${platform}.tar.gz * --remove-files
    mkdir -p $out
    cp -r * $out
  '';
}
