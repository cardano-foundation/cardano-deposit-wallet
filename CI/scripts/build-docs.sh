#!/usr/bin/env bash

# Use nix develop ./CI#docs to run this script
# The script builds the documentation for the current branch and
# pushes it to the gh-pages branch
# There are three possible targets for the documentation:
# - tagged: the documentation for any tagged commit, it will appear in a folder
#   with the tag name
# - canary: the documentation for the main branch, it will appear in a folder
#   called canary
# - test: the documentation for any other branch, it will appear in a folder
#   called test
# For new tags it is necessary to update the landing page to include a link to
# the new documentation

set -euox pipefail

# Define the current branch in case of buildkite or local
if [ -n "${BUILDKITE:-}" ]; then
	current_branch=${BUILDKITE_BRANCH}
else
	current_branch=$(git rev-parse --abbrev-ref HEAD)
fi

# Check if the current commit is tagged
current_tag=$(git describe --tags --exact-match || echo "no-tag")

if [ "$current_tag" != "no-tag" ]; then
	# If the current commit is tagged, use the tag as the target
	target="$current_tag"
elif [ "$current_branch" == "main" ]; then
	# If the current branch is main, use "latest" as the target
	target="canary"
else
	# In all other cases target is test
	target="test"
fi
temp_docs_dir=$(mktemp -d)
temp_site_dir=$(mktemp -d)

# Update the title field in book.toml to reflect the target
sed -i -r "s/^title = \"(.*)\"/title = \"\1 ($target)\"/" site/docs/book.toml

# Build the docs for the target
mdbook build site/docs -d "$temp_docs_dir"

# Build the landing page, only for the latest target
if [ "$target" == "canary" ]; then
	mdbook build site/landing -d "$temp_site_dir"
fi

# clean up and switch to gh-pages branch
git reset --hard
git fetch --all
git checkout gh-pages
git reset --hard origin/gh-pages

# populate the root of the html site with the landing mdbook, only for the latest target
if [ "$target" == "canary" ]; then
	# shellcheck disable=SC2086
	cp -r $temp_site_dir/* .
	rm -rf "$temp_site_dir"
fi

# populate the docs target folder with the mdbook of the docs
docs_target="docs/$target"
mkdir -p "$docs_target"
# shellcheck disable=SC2086
cp -r $temp_docs_dir/* "$docs_target"
rm -rf "$temp_docs_dir"

# Push the changes to the gh-pages branch
git add .
git commit -m "Update pages from $current_branch to $target" || true
git push origin gh-pages || true

# Switch back to the original branch
if [ -z "${BUILDKITE:-}" ]; then
	git checkout "$current_branch"
	git clean -fxd
fi
