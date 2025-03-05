# Roadmap: Cardano Deposit Wallet

  [cdw]: https://github.com/cardano-foundation/cardano-deposit-wallet

This roadmap describes potential directions for the [Cardano Deposit Wallet][cdw].

# Minimium Viable Product

Production readiness:

* Separate executable:
  `cardano-deposit-wallet`.
* Persistence:
  The wallet state is stored on disk and does not have to be resynchronized every time that the deposit wallet process is started.
* REST API: For interacting with the wallet.

# Future work, potential

* Formal methods

  * Prove the implementation correct with respect to the specification.

* Refunds

   * Create transaction that refunds any UTxO from a customer — but which the wallet does not accept (e.g. because they contain NFTs). The user has to pay the transaction fee.

* Staking?

  * Delegate wallet funds to a stake pool.

  * A transaction output can belong to the wallet but may have a [delegation part][addr] that points to a stake pool.

    * Users can stake with the funds as long as possible?

    * Should we make a transaction that moves those funds to a different delegation address?


  [addr]: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019#shelley-addresses
