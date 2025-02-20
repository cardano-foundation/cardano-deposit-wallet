#!/usr/bin/env bash

set -euox pipefail

VERSION=$(nix eval --raw .#version)
export VERSION

home=$(pwd)
workdir=$(mktemp -d)

cleanup() {
	cd "$home"
	rm -rf "$workdir"
	docker image rm -f cardanofoundation/cardano-deposit-wallet:"$VERSION"
}

trap cleanup EXIT
trap cleanup INT

cd "$workdir" || exit

if [ -n "${BUILDKITE:-}" ]; then
	cardano_wallet_image="result/cardano-deposit-wallet-$VERSION-docker.tar.gz"
	buildkite-agent artifact download "$cardano_wallet_image" .
else
	cardano_wallet_image="$1"
fi

# load the cardano-wallet image
docker load -i "$cardano_wallet_image"

export NETWORK=preprod
export USE_LOCAL_IMAGE=true

cp "$home"/run/docker/docker-compose.yml .
"$home"/run/docker/run.sh sync
