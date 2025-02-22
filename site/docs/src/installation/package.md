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
> ATM the invocation for the dposit wallet differs between released package and
> the unreleased one. The released package is invoked with `cardano-deposit` and
> the unreleased one is invoked with `cardano-deposit-wallet`. To be fixed after release

Now you can choose to download the latest version of the cardano-deposit-wallet
tarball or use a local package or build the package locally.

## Download the latest version of the cardano-deposit-wallet tarball or use a
local package

```bash download package
COMMAND="cardano-wallet"
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
COMMAND="cardano-deposit-wallet"
REPOSITORY="github:cardano-foundation/cardano-deposit-wallet"

if [ -z "${FLAKE:-}" ]; then
    if [ -n "${REF:-}" ]; then
        FLAKE="$REPOSITORY/$REF"
    elif [ -n "${REV:-}" ]; then
        FLAKE="$REPOSITORY/$REV"
    else
        FLAKE="$REPOSITORY"
    fi
fi
if [ -z "${SYSTEM:-}" ]; then
    SYSTEM_FLAG=""
else
    SYSTEM_FLAG="--system $SYSTEM"
fi
VERSION=$(nix eval --raw $FLAKE#version)
export WALLET_PACKAGE="result/cardano-deposit-wallet-$VERSION-$PACKAGED_FOR.tar.gz"
nix build $SYSTEM_FLAG $FLAKE#$PACKAGED_FOR.package
```

## Unpack the wallet package

Once the `WALLET_PACKAGE` is set, you can unpack it.

```bash explode package
tar xvzf "$WALLET_PACKAGE" > /dev/null
WALLET_DIR=$(pwd)
export WALLET_DIR
export PATH=$WALLET_DIR:$PATH
```

## Node database initialization

It is recommended to retrieve the node state from Mithril to avoid long
waiting times.

[Mithril node database setup](./installation/mithril.md)

## Manage processes

We are going to use `screen` to keep the processes running in the
background, but you can use any other tool you prefer.

If you are using these instructions as a script, you should install
cleanups to ensure the screen sessions are closed.

Otherwise, you can skip the next step and clean up the screen sessions
manually at the end.

```bash install cleanups
cleanup() {
    screen -S "$NODE_SESSION" -X quit || true
    screen -S "$WALLET_SESSION" -X quit || true
}
trap cleanup EXIT
trap cleanup SIGINT
```

## Start the node

Now you can start the node that comes bundled with the wallet:

```bash start node
NODE_SESSION="node-session-$(shuf -i 1000000-9999999 -n 1)"
NODE_CONFIGS="$WALLET_DIR/configs/preprod"
export NODE_CONFIGS
screen -dmS "$NODE_SESSION" cardano-node run \
    --config "$NODE_CONFIGS/config.json" \
    --database-path "$NODE_DB/preprod" \
    --socket-path "$NODE_DB/preprod/node.socket" \
    --topology "$NODE_CONFIGS/topology.json"
```

You can observe the node logs with:
```bash
screen -r "$NODE_SESSION"
```

Detach from the screen with `Ctrl-a d`.

## Start the wallet

Now you can start the wallet:

```bash start wallet
WALLET_SESSION="wallet-session-$(shuf -i 1000000-9999999 -n 1)"
DEPOSIT_PORT=$(shuf -i 1024-65000 -n 1)
export DEPOSIT_PORT
LEGACY_PORT=$(shuf -i 1024-65000 -n 1)
screen -dmS "$WALLET_SESSION" "$COMMAND" serve \
    --node-socket "$NODE_DB/preprod/node.socket" \
    --testnet "$NODE_CONFIGS/byron-genesis.json" \
    --ui-deposit-port "$DEPOSIT_PORT" \
```

> At the moment, the legacy port is required even if we are not using
> it. Once the deposit wallet is extracted from the shelley wallet
> code-base, the legacy port will be removed.

You can observe the wallet logs with:


```bash
screen -r "$WALLET_SESSION"
```

Detach from the screen with `Ctrl-a d`.

## Test the web UI

Use `curl` to check if the UI is up

```bash test home page
sleep 15
curl -s "http://localhost:$DEPOSIT_PORT" > /dev/null
```

## Open the web UI with your browser

Now you can connect to the web UI with:

```bash open web ui
xdg-open "http://localhost:$DEPOSIT_PORT"
```

## Cleanup screen sessions

Clean up the screen sessions using:

```bash clean up
screen -S "$NODE_SESSION" -X quit
screen -S "$WALLET_SESSION" -X quit
```
