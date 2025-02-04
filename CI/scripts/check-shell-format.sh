#!/usr/bin/env bash

set -euox pipefail

# Check shfmt on all *.bash files creates no changes
find . -name '*.sh' -exec shfmt -d {} +
