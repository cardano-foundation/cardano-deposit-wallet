# Problem: Origin of incoming payments

The main goal of the [Cardano Deposit Wallet][cdw] is to offer a solution for the following problem:

# Problem statement

Some wallet users, typically businesses, would like to identify the origin of incoming payments, typically from their customers.

  [cdw]: https://github.com/cardano-foundation/cardano-deposit-wallet
  [cardano-wallet]: https://github.com/cardano-foundation/cardano-wallet

# Status Quo

## Solution: Assign one address to one customer

The most commonly used solution to tracking the origin of incoming payments is the following:

- The customer creates an account with the business.

- The business generates a unique address and associates it with this customer account.

- Any incoming payment on that address is treated as originating from that customer.

The [Cardano Deposit Wallet][cdw] implements this solution.

The [cardano-wallet][] software *lacks* explicit support for this solution, but it is still often used for this purpose. See below.

### User: Centralized Cryptocurrency Exchanges (CEX)

Exchanges enable users to trade cryptocurrencies for both crypto and fiat currencies.

Typically, CEXes keep track of one account for each customer. A customer can deposit ADA in their account (“top up”) by sending it to a Cardano address that the Exchange has associated with and communicated to the customer.

CEXes use various wallet setups:

- Single wallet for both deposits and withdrawals.
- Two wallets: one for deposits, the other one for withdrawals.

In both cases, the wallet responsible for accepting deposits tracks the addresses at which the deposit was made.

### User: Small merchants

Small online stores want to know when they receive ADA from a customer who placed an order, so that they can deliver the goods.

For example, here is a feature request for [cardano-wallet][]:

* [Feature to request additional addresses for a wallet #2921](https://github.com/cardano-foundation/cardano-wallet/issues/2921)

## Support in cardano-wallet

[Cardano-wallet][] currently **lacks** explicit support for the type of usage described above. Specifically,

- The API documentation for Byron wallets and Shelley wallets implicitly allows funds to be moved between addresses — there is no guarantee that funds sent to a wallet address originate from outside the wallet, they could come from the wallet itself!

In practice, people use [cardano-wallet][] to track  funds incoming at particular addresses anyway, typically by inspecting the transaction history.

Besides the fundamental limitation above, using [cardano-wallet][] in this way has additional drawbacks:

Byron-style wallets have the following drawbacks:

- Addresses that have been created and imported cannot be restored from a single seed phrase.

* The Base58 encoding used for Byron addresses tolerates less errors than Bech32, making loss of funds more likely for customers.

* Address generation relies on implementation details of Haskell’s `StdGen` pseudorandom number generator.

Shelley-style wallet has the following drawbacks:

- Making a transaction to generate new addresses incurs a transaction fee.

- Wallets with an address gap ≠ 20 do not comply with the BIP-44 standard for account discovery; they cannot be fully restored without knowledge of the address gap.

# Alternative solutions

The problem statement could also be addressed by other means. For example:

- Transaction metadata could be augmented with identifying information about the origin of incoming payments.

- Identifying information could be added to the addresses.

## Solution: Transaction metadata

In traditional banking, transaction metadata is typically used to identify the origin of an incoming payment.

Also, for some cryptocurrencies, Exchanges do a single address and distinguish users by transaction metadata. This is the case for Kraken and the [Stellar Lumens](https://en.wikipedia.org/wiki/Stellar_\(payment_network\)) blockchain.

However, the notion of “one address — one customer” appears to be established for ADA by now, probably also because current user interfaces (such as Daedalus) lack good ways to add transaction metadata.

## Solution: Identity from sender addresses

One could require customers to send money from a specific address. However, that would require a feature where the user wallet is able to send from a specific address. To our knowledge, such a feature has not been implemented, and it seems to interfere with intentional anonymity of addresses.
