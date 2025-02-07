#! /usr/bin/env bash

set -euox pipefail

base_build=$(buildkite-agent meta-data get base-build)
VERSION=$(buildkite-agent meta-data get release-version)

main_build=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
	-X GET "https://api.buildkite.com/v2/builds" |
	jq ".[] | select(.meta_data.\"triggered-by\" == \"$base_build\")" |
	jq .number)

mkdir -p artifacts

artifact() {
	local artifact_name=$1

	# shellcheck disable=SC2155
	local artifact_value=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
		-X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-deposit-wallet-main/builds/$main_build/artifacts?per_page=100" |
		jq -r " [.[] | select(.filename == \"$artifact_name\")][0] \
	    | .download_url")
	curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L \
		-o "artifacts/$artifact_name" \
		"$artifact_value"
	gh release upload "$VERSION" "artifacts/$artifact_name"
}

artifact "cardano-deposit-wallet-$VERSION-linux64.tar.gz"
artifact "cardano-deposit-wallet-$VERSION-macos-silicon.tar.gz"
artifact "cardano-deposit-wallet-$VERSION-macos-intel.tar.gz"
artifact "cardano-deposit-wallet-$VERSION-docker.tar.gz"
