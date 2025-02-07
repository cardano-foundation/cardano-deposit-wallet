#!/usr/bin/env  bash

if [ -z "${BUILDKITE:-}" ]; then
	echo "Error: BUILDKITE is not set."
	exit 1
fi

# use BUILDKITE_BRANCH if BUILDKITE_TAG is not set
if [ -z "${BUILDKITE_TAG:-}" ]; then
	BUILDKITE_TAG=$BUILDKITE_BRANCH
fi

# make sure the buildkite tag starts with rc-
if [[ ! $BUILDKITE_TAG =~ ^rc- ]]; then
	echo "Error: BUILDKITE_TAG does not start with rc-"
	exit 1
fi

# remove rc- from the buildkite tag
VERSION=${BUILDKITE_TAG#rc-}

# use sed to update the version in the nix file
sed -i "s/version = self.dirtyShortRev or self.shortRev;/version = \"$VERSION\";/" \
	flake.nix

# configure git
git config --global user.email "hal@cardanofoundation.org"
git config --global user.name "Buildkite Pipeline"

# commit the changes
git add flake.nix
git commit -m "Bump version to $VERSION"

RELEASE_CANDIDATE_BRANCH="release-candidate/$VERSION"

# create a release candidate branch
git checkout -b "$RELEASE_CANDIDATE_BRANCH"

# push the changes
git push -f origin "$RELEASE_CANDIDATE_BRANCH"

if [ -n "${BUILDKITE:-}" ]; then
	buildkite-agent meta-data set "release-version" "$VERSION"
	buildkite-agent meta-data set "release-candidate-commit" "$BUILDKITE_COMMIT"
	buildkite-agent meta-data set "release-candidate-branch" "$RELEASE_CANDIDATE_BRANCH"
	buildkite-agent meta-data set "base-build" "$BUILDKITE_BUILD_ID"
fi
