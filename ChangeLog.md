# Changelog for wallet-proxy

## Unreleased changes
- add `GET /v1/ip_info` endpoint.
- add `--ip-data-v1` option for the JSON file that should be displayed at the above endpoint.

## 0.15.6
 - more informative error responses when the node rejects the GRPC call

## 0.15.4
 - make the format of baker pool pending changes consistent with account pending
   changes

## 0.15.3
 - add estimated cooldown also to the `bakerPool` query.

## 0.15.2
 - fix regression in account balance format introduced in 0.15.1

## 0.15.1
 - add an estimate cooldown end to the `accBalance` query.

## 0.15.0
 - add `GET /v0/epochLength` endpoint

## 0.14.0
 - add `GET /v0/appSettings` endpoint

## 0.13.5
 - add `GET /v0/passiveDelegation` endpoint
 - correct calculation of cost of the configure baker transaction

## 0.13.4
 - add `GET /v0/bakerPool/{bakerId}` endpoint
 - add `GET /v0/chainParameters` endpoint
 - add `GET /v0/nextPayday` endpoint
 - add support for delegation events and new baker and delegation transactions
   in the `submissionStatus` endpoint.
 - add support for the cost of the new baker and delegation transactions
   in the `transactionCost` endpoint.
 - add support for events triggered by V1 smart contracts

## 0.8.2
 - Rename GTU to CCD in transaction details.

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
