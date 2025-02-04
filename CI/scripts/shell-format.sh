#!/usr/bin/env bash

set -euox pipefail

# Format all *.sh files with shfmt
find . -name '*.sh' -exec shfmt -w {} +
