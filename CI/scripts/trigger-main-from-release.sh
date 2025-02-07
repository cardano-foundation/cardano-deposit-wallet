#! /usr/bin/env bash

set -euo pipefail

COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")
VERSION=$(buildkite-agent meta-data get "release-version")
BASE_BUILD=$(buildkite-agent meta-data get "base-build")

title="Release Candidate of $VERSION"

cat <<YAML
steps:
  - trigger: cardano-deposit-wallet-main
    key: main-pipeline-build
    label: Main pipeline on release candidate $VERSION
    build:
        commit: $COMMIT
        branch: $VERSION
        message: $title
        env:
            RELEASE_CANDIDATE: "$VERSION"
        meta_data:
            release-version: "$VERSION"
            release-candidate-commit: "$COMMIT"
            triggered-by: "$BASE_BUILD"

YAML
