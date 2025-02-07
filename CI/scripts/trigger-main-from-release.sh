#! /usr/bin/env bash

set -euo pipefail

COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")
VERSION=$(buildkite-agent meta-data get "release-version")
BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")
BASE_BUILD=$(buildkite-agent meta-data get "base-build")

title="Release Candidate of $VERSION"

cat <<YAML
steps:
  - trigger: cardano-deposit-wallet
    key: main-pipeline-build
    label: Trigger the main pipeline
    build:
        commit: $COMMIT
        branch: $BRANCH
        message: $title
        env:
            RELEASE_CANDIDATE: "$VERSION"
        meta_data:
            release-version: "$VERSION"
            release-candidate-commit: "$COMMIT"
            release-candidate-branch: "$BRANCH"
            triggered-by: "$BASE_BUILD"

YAML
