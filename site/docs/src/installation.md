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

