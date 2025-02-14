#!/usr/bin/env bash

set -euox pipefail

# make sure PACKAGED_FOR is set
if [ -z "${PACKAGED_FOR:-}" ]; then
	echo "Error: PACKAGED_FOR is not set."
	exit 1
fi

# trap exit and interrupt signals
cleanup() {
	# remove screen sessions
	screen -S "${node_session}" -X quit
	screen -S "${wallet_session}" -X quit
	# end of remove screen sessions
	cd "$home"
	rm -rf "$workdir"
}
trap cleanup EXIT
trap cleanup INT

home=$(pwd)
workdir=$(mktemp -d)
node_session="node-session-$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13)"
wallet_session="wallet-session-$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13)"

# download the cardano-wallet package
if [ -n "${BUILDKITE:-}" ]; then
	VERSION=$(nix eval --raw .#version)
	cardano_wallet_segment="cardano-deposit-wallet-$VERSION-$PACKAGED_FOR"
	cardano_wallet_tar="result/$cardano_wallet_segment.tar.gz"
	buildkite-agent artifact download "$cardano_wallet_tar" "."
else
	cardano_wallet_tar="$1"
fi

# extract the cardano-wallet package
tar xvzf "$cardano_wallet_tar" -C "$workdir"

cd "$workdir"

# start the node
NODE_CONFIGS="$workdir/configs/preprod"
NODE_DB="$workdir/db"
mkdir -p "$NODE_DB/preprod"
export NODE_CONFIGS

screen -L -Logfile "$home/node.log" -dmS "${node_session}" ./cardano-node run \
	--config "$NODE_CONFIGS/config.json" \
	--database-path "$NODE_DB/preprod" \
	--socket-path "$NODE_DB/preprod/node.socket" \
	--topology "$NODE_CONFIGS/topology.json"

# start the wallet
DEPOSIT_PORT=$(shuf -i 1024-65000 -n 1)
export DEPOSIT_PORT
LEGACY_PORT=$(shuf -i 1024-65000 -n 1)
screen -L -Logfile "$home/wallet.log" -dmS "${wallet_session}" ./cardano-wallet serve \
	--node-socket "$NODE_DB/preprod/node.socket" \
	--testnet "$NODE_CONFIGS/byron-genesis.json" \
	--ui-deposit-port "$DEPOSIT_PORT" \
	--port "$LEGACY_PORT"

# wait for the wallet and the node to settle
sleep 15

# check the wallet is syncing
STATUS=$(wget -qO- "http://localhost:$LEGACY_PORT/v2/network/information" | jq .sync_progress.status)

# check the status
if [ "$STATUS" != "\"syncing\"" ]; then
	echo "Error: Wallet was not syncing. Status: $STATUS"
	exit 1
else
	echo "Wallet was syncing."
fi
