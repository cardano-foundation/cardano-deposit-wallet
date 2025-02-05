# shellcheck shell=bash

# Format the scripts
format:
  nix develop ./CI#checks -c ./CI/scripts/nix-format.sh
  nix develop ./CI#checks -c ./CI/scripts/shell-format.sh
