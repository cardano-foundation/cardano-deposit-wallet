#! /usr/bin/env bash

set -euox pipefail

VERSION=$(buildkite-agent meta-data get release-version)
title="Release Candidate of $VERSION"

gh release create \
	-d \
	-F "$TEMPLATE" \
	-t "$title" \
	"$VERSION"
