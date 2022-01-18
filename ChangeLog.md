# Changelog for wallet-proxy

## 0.8.1
 - Add the data field to a transaction list response when the transaction is a
   register data transaction.

## 0.8.0
 - Fix bug where encrypted transfers with memo were not included in transaction
   list when the encrypted filter was applied.
 - Do not create gtu drop table in the database if not configured for GTU drop.
 - Add support for account aliases.


## 0.7.0

 - Add support for transfers with a memo.
   - New endpoint v1/accTransactions which lists transfers with a memo.
   - v0/transactionCost endpoint is updated to support new transfer types. The
     change is backwards compatible.
   - Existing v0/accTransactions endpoint converts outcomes of new transaction
     types to old ones.
   - Minimum supported node version is bumped to 1.2.

## 0.6.0

 - Disable sessions since we don't use them.
 - Add query parameter `includeRawRejectReason` to the `accTransactions` query.
 - Health query checks connections to database and GRPC, and that the last final
   block is less than `health-tolerance` seconds old. `health-tolerance` is 300
   seconds unless an alternative value is chosen at startup.
 - Add filtering options `blockTimeFrom`, `blockTimeTo`, `blockRewards`, 
  `finalizationRewards`, `bakingRewards`, and  `onlyEncrypted` to the 
  `accTransactions` query.

## 0.5.0
 - Make the GTU drop functionality optional.
