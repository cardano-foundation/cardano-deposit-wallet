#!/usr/bin/env bash

set -euox pipefail

# Determine VERSION from tag, branch, or flake.nix
if [[ -n "${INPUT_VERSION:-}" ]]; then
	# workflow_dispatch with explicit version input
	VERSION="$INPUT_VERSION"
elif [[ -n "${GITHUB_REF_NAME:-}" ]]; then
	case "$GITHUB_REF_NAME" in
	rc-*)
		VERSION=v${GITHUB_REF_NAME#rc-}
		;;
	v*)
		VERSION=$GITHUB_REF_NAME
		;;
	*)
		VERSION=$(nix eval --raw .#version)
		;;
	esac
else
	VERSION=$(nix eval --raw .#version)
fi

RELEASE_CANDIDATE_BRANCH="rc/$VERSION"

git branch -D "$RELEASE_CANDIDATE_BRANCH" || true
git checkout -b "$RELEASE_CANDIDATE_BRANCH" || true

# use sed to update the version in the nix file
sed -i "s/version = self.dirtyShortRev or self.shortRev;/version = \"$VERSION\";/" \
	flake.nix

# configure git
git config --global user.email "hal@cardanofoundation.org"
git config --global user.name "GitHub Actions"

# commit the changes
git add flake.nix
git commit -m "Bump version to $VERSION"

RELEASE_CANDIDATE_COMMIT=$(git rev-parse HEAD)

git push -f origin "$RELEASE_CANDIDATE_BRANCH"

# Export outputs for GitHub Actions
if [ -n "${GITHUB_OUTPUT:-}" ]; then
	echo "version=$VERSION" >>"$GITHUB_OUTPUT"
	echo "rc-commit=$RELEASE_CANDIDATE_COMMIT" >>"$GITHUB_OUTPUT"
fi
