# Installation

This document describes how to install and start the Cardano Deposit
Wallet.

The instructions in this document are tested automatically as part of
our continuous integration.

## System Prerequisites

This is a full node experience, so all the caveats of running a full node apply.

In particular, for `mainnet` you will need:

- 24 GB RAM (UTxO set is still in memory)
- 250 GB disk space (Transaction history is stored in full)
- 4 CPU cores (for syncing)

But for `preprod`, you can get away with:

- 2 GB RAM
- 5 GB disk space
- 2 CPU cores

You will need some tools to complete the installation:

- jq
- wget
- curl
- tar
- screen
- a working browser

Nix users can run a shell with all the tools:

```bash
nix-shell -p jq wget gnutar screen curl
```
or

```bash
nix shell nixpkgs#jq nixpkgs#wget nixpkgs#gnutar nixpkgs#screen nixpkgs#curl
```

> We are going to use the preprod network in these instructions.
> Small changes are needed to run on other networks.

## Create a directory for the node state or use the current directory

```bash node-db directory
NODE_DB=${NODE_DB:-$(pwd)/node-db}
export NODE_DB
mkdir -p "$NODE_DB"
rm -rf "$NODE_DB/preprod"
```

Now you have two choices on how to proceed:

## Use a package for your system.
Not all platforms are supported, but we have packages for `linux64`,
`macos-silicon`, and `macos-intel`.

- [Using a platform package](./installation/package.md). As we do not
    ship for Windows, you will have to use a Docker solution.

## Using a docker image

The Docker image is built through the Nix package manager and has no
base image.

- [Using a docker image](./installation/docker.md)

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
screen -dmS "$WALLET_SESSION" cardano-wallet serve \
    --node-socket "$NODE_DB/preprod/node.socket" \
    --testnet "$NODE_CONFIGS/byron-genesis.json" \
    --ui-deposit-port "$DEPOSIT_PORT" \
    --port "$LEGACY_PORT"
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
