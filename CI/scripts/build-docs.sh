#!/usr/bin/env bash

set -euox pipefail

if [ -n "${BUILDKITE:-}" ]; then
    current_branch=${BUILDKITE_BRANCH}
else
    current_branch=$(git rev-parse --abbrev-ref HEAD)
fi
current_tag=$(git describe --tags --exact-match || echo "no-tag")

if [ "$current_tag" != "no-tag" ]; then
  target="docs/$current_tag"
elif [ "$current_branch" == "main" ]; then
  target="docs/latest"
else
  target="docs/test"
fi
temp_dir=$(mktemp -d)

nix develop path:CI#docs -c mdbook build site/docs -d "$temp_dir"

git fetch --all
git checkout gh-pages
git reset --hard origin/gh-pages
git clean -fdx
mkdir -p "$target"
# shellcheck disable=SC2086
cp -r $temp_dir/* "$target"
git add .
git commit -m "Update pages from $current_branch to $target"
git push origin gh-pages

rm -rf "$temp_dir"
git checkout -f "$current_branch"
git clean -fdx