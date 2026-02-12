#!/usr/bin/env bash

set -euox pipefail

if [ -z "${VERSION:-}" ]; then
	echo "Error: VERSION is not set."
	exit 1
fi

title="Release Candidate of $VERSION"

gh release create \
	-d \
	-F "$TEMPLATE" \
	-t "$title" \
	"$VERSION"
