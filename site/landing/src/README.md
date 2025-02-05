# Introduction

The Cardano Deposit Wallet is a full-node wallet targeting businesses that need to track the origin of incoming payments ("deposits").

For example, consider an online shop that accepts payments in ADA.
Whenever the shop sells an item, the shop needs to monitor the [Cardano blockchain][cardano]
for a transaction that contains the payment from the customer.
The Cardano Deposit Wallet takes care of that.

The Cardano Deposit Wallet works as follows:

* To each customer of the business, the wallet assigns a unique address.
* Whenever someone makes a deposit to that address, this deposit is assumed to originate from the customer assigned to that address.
* Outgoing funds are not distinguished by customer — once funds have gone into the wallet, they are all part of a single wallet balance.

The Cardano Deposit Wallet works with a [cardano-node][] in order to enjoy the full security and decentralization of the Cardano blockchain.

  [cardano]: https://cardano.org/
  [cardano-node]: https://github.com/IntersectMBO/cardano-node

# Site

This site presents and documents
the Cardano Deposit Wallet.
Use the sidebar for navigation.
