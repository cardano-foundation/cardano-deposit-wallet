You can use 2 different methods to run the cardano-deposit-wallet in a docker container.
1. [Use the latest release image from docker-hub as described](#get-latest-docker-image-released)
2. [Build the docker image locally from a commit on the repository as described](#build-the-docker-image-locally-from-a-commit-on-the-repository)

Once you have the image you will need to [download the docker-compose](#download-the-docker-compose-file)
and [setup the environment](#setup-the-environment).

After that you can [start the service](#start-the-service) and check that the
[wallet is working](#check-the-wallet-is-working) or use the [web UI](#open-wallet-ui).
Instructions here are machine checked and so contains some bash code to execute
clean-ups which are not necessary for manual execution.

## Use the latest release image from docker-hub

### Get latest docker-image released

```bash get latest tag
tag=$(curl -s https://api.github.com/repos/cardano-foundation/cardano-deposit-wallet/releases/latest | jq -r '.tag_name')
export WALLET_TAG=$tag
```

### Pull the docker image

```bash
docker pull cardanofoundation/cardano-deposit-wallet:$tag
```

Or, alternatively you can build the docker image locally.

## Build the docker image locally from a commit on the repository

You will need a working nix environment to build the docker image. More
information on how to install nix can be found [here](https://nixos.org/download.html).
A solution that runs nix in a docker container will come soon.

### Select the flake version
To compile different commits directly from the repository,
you can set `REV` to the commit hash.

```bash select flake version
REPOSITORY="github:cardano-foundation/cardano-deposit-wallet"

if [ -z "${REV:-}" ]; then
    DEFAULT_FLAKE_URL="$REPOSITORY?REF=main"
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
```

### Build the docker image

```bash build docker image
image="cardano-deposit-wallet-$VERSION-docker.tar.gz"

nix build $FLAKE#docker-image --out-link "result/$image"
```

### Load the docker image

```bash load docker image
docker load -i "result/$image"
export WALLET_TAG=$VERSION
```

## Download the docker-compose file

```bash download docker-compose
curl -s -o docker-compose.yml https://raw.githubusercontent.com/cardano-foundation/cardano-deposit-wallet/$WALLET_TAG/run/docker/docker-compose.yml
```

## Setup the environment

Because the `docker-compose.yaml` is parameterized, you will need to set the
environment variables before starting the service. You will need to have the
variables set in the environment to run any `docker compose` command.

```bash setup environment
export NETWORK=preprod
export USE_LOCAL_IMAGE=true
export DEPOSIT_WALLET_UI_PORT=${DEPOSIT_WALLET_UI_PORT:-4164}
export WALLET_PORT=${WALLET_PORT:-40036}
export NODE_SOCKET_DIR=${NODE_SOCKET_DIR:-.}
export NODE_DB=${NODE_DB:-./node-db}
export WALLET_DB=${WALLET_DB:-./wallet-db}
export NODE_SOCKET_NAME=${NODE_SOCKET_NAME:-node.socket}
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)

```

## Create the directories

```bash create directories
mkdir -p $WALLET_DB
rm -rf $WALLET_DB/*
mkdir -p $NODE_DB
rm -rf $NODE_DB/*
```

## Auto cleanup

In case you are using this instruction as a test script you want to set up the
cleanup of the environment otherwise you can skip this step.

```bash auto cleanup
cleanup() {
    docker compose down || true
    rm -rf $WALLET_DB
    rm -rf $NODE_DB
    rm -f docker-compose.yml
}
trap cleanup EXIT
```

## Mithril node state preload (optional)

It's critical that we target the right network with the variables. In this case,
we are targeting the preprod network.
See [Mitrhil documentation](https://mithril.network/doc/manual/getting-started/network-configurations)
for more information.


```bash mithril node state preload
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d
export MITHRIL_TAG=2450.0-c6c7eba
digest=$(docker compose --profile mithril run --rm mithril cdb snapshot list --json | jq -r .[0].digest)
docker compose --profile mithril run --rm mithril cdb download "$digest"
```
## Start the service

```bash start service
docker compose up -d
```

## Check the wallet is working

```bash check wallet
sleep 10 # give some time for the containers to start
if ! curl -s http://localhost:$DEPOSIT_WALLET_UI_PORT | grep -q "Deposit Cardano Wallet";
then
    echo "Deposit Wallet is not running"
    exit 1
fi
```

## Open wallet UI

Instructions on the web-UI can be found [here](../usage.md).

```bash open wallet ui
xdg-open http://localhost:$DEPOSIT_WALLET_UI_PORT
```

## Inspect the logs

```bash
docker compose logs -f
```

## Stop the containers

```bash stop service
docker compose down
```

## Manual cleanup

```bash cleanup
rm -rf $WALLET_DB
rm -rf $NODE_DB
rm -f docker-compose.yml
```
