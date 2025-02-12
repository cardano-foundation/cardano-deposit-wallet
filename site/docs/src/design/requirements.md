# Requirements: Deposit Wallet

  [cdw]: https://github.com/cardano-foundation/cardano-deposit-wallet
  [cardano-wallet]: https://github.com/cardano-foundation/cardano-wallet

# Synopsis

This documents describes high-level requirements on the [Cardano Deposit Wallet][cdw].

The [Cardano Deposit Wallet][cdw] is a full-node wallet targeting businesses that need to track the origin of incoming payments ("deposits").

Such a wallet is useful to businesses who want to match incoming payments to customers. The matching works as follows:

- The customer creates an account with the business.
- In the wallet, the business generates a unique address and associates it with this customer account.
- Any incoming payment on that address is treated as originating from that customer.

# Motivation

See [Problem statement](./problem.md).

# Requirements

## Synopsis

Compared to [cardano-wallet][], the Deposit Wallet

- Officially supports a mapping between addresses to customers.

- Improves performance when handling many addresses and large UTxO sets.

- Supports a larger number of customers (up to 2³¹ \~ 2.1 Billion) in a single wallet.

- Uses the Bech32 encoding for customer addresses, which is more resilient against spelling mistakes.

- Basic User Interface for Wallet Usability and Demonstration.

## Details

### Main

**Known customers**:

  * The wallet maintains a one-to-one mapping between customers and addresses.

  * Customers are represented as numerical indices starting at `0`.

  * The maximum number of customers is at leaset 2³¹ \~ 2.1 Billion.

  * Addresses are encoded using Bech32, which is more resilient against spelling mistakes.

  * Addresses are derived deterministically from a public key using [BIP-32][]-style key derivation.

Incoming transactions:

  * A deposit made at an address is treated as originating from the corresponding customer.

  * We can **query** the wallet for a recent history of deposits made by each known customer.

Outgoing transactions:

  * Outgoing funds are not distinguished by customer — once funds have gone into the wallet, they are all part of a single wallet balance.

  * A transaction created by the wallet does **not send** funds to a **known customer** unless the wallet user explicitly wants that customer as payment destination.

  [bip-32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki

Signatures:

  * The wallet stores a public key from which customer addresses are derived deterministically.

  * Optionally, the wallet stores a private key in order to sign outgoing transactions. This private key is encrypted with a passphrase.

### Technical

Incoming transactions:

  * Rollbacks of the blockchain are supported and do not cause an inconsistent wallet state.

  * The query for the history of deposits is atomic with respect to rollbacks.

  * Variant queries for the history of deposits are supported

Outgoing transactions:

  * The wallet can create a valid transaction if sufficient funds are available.

  * The wallet can sign the transaction if the optional private key is stored.

API:

  * REST API that exposes the wallet functionality

  * web UI for testing purposes

### Non-Functional

Performance

  * The wallet state is persisted to disk and does not need to be recreated by synchronizing to the blockchain.

  * Time-complexity of reading a new block should be

    * logarithmic in the size of the address↔︎customer mapping

    * logarithmic in the size of the current wallet UTxO
