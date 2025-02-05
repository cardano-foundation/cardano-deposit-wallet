#!/usr/bin/env bash

set -euox pipefail

# Format all *.nix files with nixfmt
find . -name '*.nix' -exec nixfmt {} +
