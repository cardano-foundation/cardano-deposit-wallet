#!/usr/bin/env  bash

set -euox pipefail

if [ -z "${BUILDKITE:-}" ]; then
	echo "Error: BUILDKITE is not set."
	exit 1
fi

# use BUILDKITE_BRANCH if BUILDKITE_TAG is not set, fallback to the version in the flake.nix
if [[ -z "${BUILDKITE_TAG:-}" ]]; then
	if [[ ! $BUILDKITE_BRANCH =~ ^rc- ]]; then
		VERSION=$(nix eval --raw .#version)
	else
		VERSION=${BUILDKITE_BRANCH#rc-}
	fi
elif
	# check buildkite tag does not starts with rc-
	[[ ! $BUILDKITE_TAG =~ ^rc- ]]
then
	# use the buildkite tag as the version
	VERSION=$BUILDKITE_TAG
else
	# remove rc- from the buildkite tag, this is a release candidate
	VERSION=${BUILDKITE_TAG#rc-}
fi

# use sed to update the version in the nix file
sed -i "s/version = self.dirtyShortRev or self.shortRev;/version = \"$VERSION\";/" \
	flake.nix

# configure git
git config --global user.email "hal@cardanofoundation.org"
git config --global user.name "Buildkite Pipeline"

# commit the changes
git add flake.nix
git commit -m "Bump version to $VERSION"

if [ -n "${BUILDKITE:-}" ]; then
	buildkite-agent meta-data set "release-version" "$VERSION"
	buildkite-agent meta-data set "release-candidate-commit" "$BUILDKITE_COMMIT"
	buildkite-agent meta-data set "base-build" "$BUILDKITE_BUILD_ID"
fi
