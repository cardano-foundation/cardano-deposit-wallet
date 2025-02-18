#!/usr/bin/env bash

set -euox pipefail

cwd=$(pwd)
dir=$(mktemp -d -t ci-XXXXXXXXXX)
script="$dir/run.sh"

cleanup() {
	cd "$cwd" || exit
	rm -rf "$dir"
}
trap cleanup EXIT
trap cleanup INT

if [ -z "${LOGGING:-}" ]; then
	LOGGING=""
else
	LOGGING="-l"
fi

recipe="$1"

nix run ./CI/scripts/runmd#runmd -- $LOGGING -r "$recipe" -d ./site/docs/src >"$script"

chmod +x "$script"

cd "$dir" || exit
./run.sh
