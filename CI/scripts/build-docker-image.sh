#!/usr/bin/env bash

set -euox pipefail

VERSION=$(nix eval --raw .#version)

image="cardano-deposit-wallet-$VERSION-docker.tar.gz"

nix build .#docker-image --out-link "result/$image"
