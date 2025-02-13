# Get a usable cardano-deposit-wallet package

To use the following instructions you will need to set `PACKAGED_FOR` to the
platform you are using:

```bash
export PACKAGED_FOR=linux64
```

```bash
export PACKAGED_FOR=macos-silicon
```

```bash
export PACKAGED_FOR=macos-intel
```

Now you can choose to download the latest version of the cardano-deposit-wallet
tarball or use a local package or build the package locally.

## Download the latest version of the cardano-deposit-wallet tarball or use a
local package

```bash download package
WALLET_VERSION=$(curl -s https://api.github.com/repos/cardano-foundation/cardano-deposit-wallet/releases/latest | jq -r .tag_name)
WALLET_PACKAGE=cardano-deposit-wallet-$WALLET_VERSION-$PACKAGED_FOR.tar.gz
wget -q https://github.com/cardano-foundation/cardano-deposit-wallet/releases/download/$WALLET_VERSION/$WALLET_PACKAGE
export WALLET_PACKAGE
```
Alternatively

## Build the package locally

Set the `FLAKE` variable if you are working on a local clone, or leave it as
it is to use the GitHub repository directly at main branch.

To compile different branches directly from the repository, you can set `REF`
to the branch name. To compile different commits directly from the repository,
you can set `REV` to the commit hash.

You may want to override the `SYSTEM` variable if you are compiling for
different processor architectures on macOS.

`SYSTEM` can be set to `x86_64-darwin`, or `aarch64-darwin`. If not set it
will default to the current system.

```bash build package
REPOSITORY="github:cardano-foundation/cardano-deposit-wallet"

if [ -z "${REF:-}" ]; then
    DEFAULT_FLAKE_URL="$REPOSITORY"
elif [ -z "${REV:-}" ]; then
    DEFAULT_FLAKE_URL="$REPOSITORY?ref=$REF"
else
    DEFAULT_FLAKE_URL="$REPOSITORY?rev=$REV"
fi

FLAKE=${FLAKE:-$DEFAULT_FLAKE_URL}

if [ -z "${SYSTEM:-}" ]; then
    SYSTEM_FLAG=""
else
    SYSTEM_FLAG="--system $SYSTEM"
fi
VERSION=$(nix eval --raw $FLAKE#version)
export WALLET_PACKAGE="result/cardano-deposit-wallet-$VERSION-$PACKAGED_FOR.tar.gz"
nix build $SYSTEM_FLAG $FLAKE#$PACKAGED_FOR.package
```
