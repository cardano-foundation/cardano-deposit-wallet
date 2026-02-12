#!/usr/bin/env bash

set -euox pipefail

if [ -z "${VERSION:-}" ]; then
	echo "Error: VERSION is not set."
	exit 1
fi

# Artifacts are downloaded by actions/download-artifact into ./artifacts/
for f in artifacts/*.tar.gz; do
	gh release upload "$VERSION" "$f"
done
