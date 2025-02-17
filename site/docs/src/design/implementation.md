# Implementation: Customer Deposit Wallet

  [cdw]: https://github.com/cardano-foundation/cardano-deposit-wallet

This document describes selected aspects of the current implementation of the [Cardano Deposit Wallet][cdw].

We focus on aspects that are about the overall organization of the source code and cannot be learned by looking at individual files.

TODO: This document needs to become part of the source code.

# Repositories

The source code for the [Cardano Deposit Wallet][cdw] is currently split over several repositories and packages:

* Repo [cardano-wallet-agda][]

  * Package [customer-deposit-wallet-pure][]
    * Specification.
    * Implementation of main pieces of functionality using [Agda2hs][], in a purely functional style.

  * Package [customer-deposit-wallet-pure][] — helper library. Also exported to [agda2hs][].

* Repo [cardano-wallet][]

  * Package [customer-deposit-wallet][] — Package that assembles the pieces of functionality from [customer-deposit-wallet-pure][] and makes them useable in imperative styles.

  * Package [customer-deposit-wallet-ui][] — Package that presents a web UI for the Deposit Wallet.

  * Package `network-layer`, … — helper packages.

  * Executable `cardano-wallet` — currently contains both the legacy `cardano-wallet` and the Deposit Wallet with UI.

* Repo [cardano-deposit-wallet][]

  * Provides releases containing the executable.

  * Documentation.

  [agda2hs]: https://github.com/agda/agda2hs
  [cardano-wallet]: https://github.com/cardano-foundation/cardano-wallet
  [cardano-wallet-agda]: https://github.com/cardano-foundation/cardano-wallet-agda
  [customer-deposit-wallet]: https://github.com/cardano-foundation/cardano-wallet/tree/master/lib/customer-deposit-wallet
  [customer-deposit-wallet-ui]: https://github.com/cardano-foundation/cardano-wallet/tree/master/lib/ui
  [customer-deposit-wallet-pure]: https://github.com/cardano-foundation/cardano-wallet-agda/tree/main/lib/customer-deposit-wallet-pure
  [cardano-deposit-wallet]: https://github.com/cardano-foundation/cardano-deposit-wallet/

# Conformance to Specification

The package [customer-deposit-wallet-pure][] contains both the specification and and implementation of various aspects of functionality. Some of these aspects come with proofs. An experimental implementation combines the aspects and proves a few properties of the Specification.

However, the actual implementation in [customer-deposit-wallet][] has not been proven or tested correct with respect to the Specification yet.

# Module Organization

The package [customer-deposit-wallet][] is organized into modules with the prefix `Cardano.Wallet.Deposit.*`.

In turn, this prefix is organized into groups of modules. These groups build on top of each other — each group wraps the previous group, moving from purely functional style to an imperative style, with more and more integration with the operating system and other interfaces.

The succession of module groups is:

1. `Pure` — Implementation in purely functional style: only data types and pure functions on it.
2. `IO` — Straightforward wrapper of the pure functions into an interface with implicit control flow (mutable state, exceptions, concurrency).
3. `REST` — Straightforward wrapper of `IO` into an API that aligns with REST principles.
4. `HTTP` — Straightforward wrapper of `REST` into an HTTP API.

The package [customer-deposit-wallet-ui][] add the group

4'. `UI` — Presentation of `REST` as a web UI.
