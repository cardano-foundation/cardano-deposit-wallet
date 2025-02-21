#!/usr/bin/env bash

# Enforce strict script execution modes
set -euox pipefail

# Function to display usage information
usage() {
	echo "Usage: $0 [sync|start|stop|logs|help|node-logs|wallet-logs]"
	echo "  sync: Sync the service and wait for it to be ready"
	echo "  start: Start the service"
	echo "  stop: Stop the service"
	echo "  help: Show this help message"
	echo "  node-logs: Show the logs of the node service"
	echo "  wallet-logs: Show the logs of the wallet service"
}

# Check if no arguments are provided and display usage if true
if [ $# -eq 0 ]; then
	usage
	exit 1
fi

# enforce NETWORK variable
if [ -z "${NETWORK-}" ]; then
	echo "NETWORK variable is not set"
	exit 1
fi

# enforce VERSION variable
if [ -z "${VERSION-}" ]; then
	echo "VERSION variable is not set"
	exit 1
fi

# Define and export wallet and node version tags
RELEASE_WALLET_TAG=$VERSION

WALLET_TAG=${WALLET_TAG:=$RELEASE_WALLET_TAG}
export WALLET_TAG

NODE_TAG=10.1.4
export NODE_TAG

# Generate a random port for the wallet service and export it
RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_PORT=${WALLET_PORT:=$RANDOM_PORT}
export WALLET_PORT

RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
DEPOSIT_WALLET_UI_PORT=${DEPOSIT_WALLET_UI_PORT:=$RANDOM_PORT}
export DEPOSIT_WALLET_UI_PORT

# Define a local db if WALLET_DB is not set
if [[ -z "${WALLET_DB-}" ]]; then
	LOCAL_WALLET_DB=./databases/wallet-db
	mkdir -p $LOCAL_WALLET_DB
	WALLET_DB=$LOCAL_WALLET_DB
	export WALLET_DB
fi
rm -rf "${WALLET_DB:?}/*"

# Define a local db if NODE_DB is not set
if [[ -z "${NODE_DB-}" ]]; then
	LOCAL_NODE_DB=./databases/node-db
	NODE_DB=$LOCAL_NODE_DB
	export NODE_DB
fi
mkdir -p "${NODE_DB:?}"

# Get the current user's ID and export it
USER_ID=$(id -u)
export USER_ID

# Get the current user's group ID and export it
GROUP_ID=$(id -g)
export GROUP_ID

# Define and export the node socket name
NODE_SOCKET_NAME=node.socket
export NODE_SOCKET_NAME

# Define and export the local and actual directory for the node socket
LOCAL_NODE_SOCKET_DIR=./databases/node-db
NODE_SOCKET_DIR=${NODE_SOCKET_DIR:=$LOCAL_NODE_SOCKET_DIR}
export NODE_SOCKET_DIR

startup() {
	# Pull the latest images
	if [ -z "${USE_LOCAL_IMAGE-}" ]; then
		docker compose pull -q
	fi
	# Start the service in detached mode
	docker compose up -d
}

# Function to clean up the service
cleanup() {
	echo "Cleaning up..."
	docker compose down 2>/dev/null
	sleep 3
	docker compose kill 2>/dev/null
}

# Case statement to handle different command-line arguments
case "$1" in
sync)

	echo "Wallet service tag: $WALLET_TAG"
	echo "Deposit wallet ui port: $DEPOSIT_WALLET_UI_PORT"
	echo "Syncing the service..."
	startup

	# Initialize timeout and start time for the sync operation
	timeout=200
	start_time=$(date +%s)

	# Commands to query service status and node tip time
	command=$(printf "docker run --network %s_default alpine/curl curl -s --max-time 3 cardano-deposit-wallet:8092/data/sse | head -n1 || true" "$NETWORK")

	# Execute and display the full query result
	trap cleanup ERR INT

	SUCCESS_STATUS=${SUCCESS_STATUS:="event: tip"}

	while true; do
		# Check the sync status
		status=$(cat <(bash -c "$command")) || echo "failed"
		if [[ $(date +%s) -ge $((start_time + timeout)) ]]; then
			result="timeout"
			break
		elif [[ "$status" == "$SUCCESS_STATUS" ]]; then
			result="success"
			printf "\n"
			break
		else
			sleep 1
		fi
	done
	cleanup

	# Stop the service after syncing
	echo "Result: $result"
	# Exit with 0 on success, 1 on failure or timeout
	if [[ "$result" == "success" ]]; then
		exit 0
	else
		exit 1
	fi
	;;
start)
	echo "Starting the service..."
	echo "Deposit wallet ui port: $DEPOSIT_WALLET_UI_PORT"
	echo "Wallet service tag: $WALLET_TAG"
	startup
	;;
stop)
	echo "Stopping the service..."
	cleanup
	;;
node-logs)
	echo "Showing logs..."
	docker compose logs -f cardano-node
	;;
wallet-logs)
	echo "Showing logs..."
	docker compose logs -f cardano-deposit-wallet
	;;
node-db-with-mithril)
	node-db-with-mithril
	;;
help)
	usage # Display usage information
	;;
*)
	echo "Error: Invalid option '$1'"
	usage # Display usage information and exit with error
	exit 1
	;;
esac
