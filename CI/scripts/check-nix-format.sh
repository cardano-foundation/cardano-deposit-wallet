#!/usr/bin/env bash

set -euox pipefail

# Check nixfmt on all *.nix files creates no changes
find . -name '*.nix' -exec nixfmt --check {} +