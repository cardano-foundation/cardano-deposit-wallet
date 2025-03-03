---
sidebar_label: 'With Mithril'
sidebar_position: 1
---

## Download the Mithril executable

```bash download mithril
case $PACKAGED_FOR in
    linux64)
        MITHRIL_PLATFORM=linux-x64
        ;;
    macos-silicon)
        MITHRIL_PLATFORM=macos-arm64
        ;;
    macos-intel)
        MITHRIL_PLATFORM=macos-x64
        ;;
    *)
        echo "Unsupported platform: $PACKAGED_FOR"
        exit 1
        ;;
esac
LATEST_MITHRIL=$(curl -s https://api.github.com/repos/input-output-hk/mithril/releases/latest | jq -r .tag_name)
MITHRIL_NAME=mithril-$LATEST_MITHRIL-$MITHRIL_PLATFORM
wget -q https://github.com/input-output-hk/mithril/releases/download/$LATEST_MITHRIL/$MITHRIL_NAME.tar.gz
MITHRIL_DIR=$(pwd)/$MITHRIL_NAME
mkdir -p "$MITHRIL_DIR"
tar xvzf "$MITHRIL_NAME.tar.gz" -C "$MITHRIL_DIR" > /dev/null
rm -f "$MITHRIL_NAME.tar.gz"
export PATH="$MITHRIL_DIR":$PATH
```

Test that mithril is installed correctly by running `mithril --version`.

```bash test mithril
mithril-client --version
```

> NixOS will refuse to run Mithril unless `programs.nix-ld.enable = true;` is in the configuration.

## Populate the node state with preprod data

```bash populate preprod
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d
digest=$(mithril-client cdb snapshot list --json | jq -r .[0].digest)
(cd "${NODE_DB}" && mithril-client cdb download "$digest" && mv db preprod)
```
