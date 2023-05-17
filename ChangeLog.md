# Changelog for wallet-proxy

## Unreleased changes

## 0.26.3

- Fix bug in CIS2TokenBalance endpoint where it sometimes returned an incorrect
  amount due to overflow.

## 0.26.2

- Revert behaviour in case when accounts are not found. Now, again, a status
  code 200 is returned with an empty object.

## 0.26.1

- Fix bug in transactionCost which expected a block hash header in response.

## 0.26

- The `--grpc-authentication-token` option has been removed.
- The wallet proxy now uses the node GRPC API V2 to interact with the node.
  The port specified with the `--grpc-port` option must therefore now be one
  on which this is served. Since the default port on which the V2 GRPC API is
  served by a node is 20000, the default value of this option has been updated
  to reflect this.

## 0.25.1

- Fix displaying `message` in contract updates. Use hex consistently.

## 0.25.0

- Add `GET /v0/termsAndConditionsVersion` endpoint and the corresponding
  configuration options `--tc-version`, and `--tc-url` which default to 1.0.0
  and
  https://developer.concordium.software/en/mainnet/net/resources/terms-and-conditions-bw.html 
  if not set.

## 0.23.0

- Expose "contract name" in the CIS2TokenMetadata response.

## 0.22.2

- Make the closing of connection to the node more robust when reconnecting. In
  particular exceptions raised during the closing of the connection no longer
  lead to an irrecoverable state like they used to in certain scenarios.

## 0.22.1

- Display receive function parameters as hex strings in transactions.
- Fix reconnect handling when retryNum is set to 1. When handling the GOAWAY
  message the reconnect is not successful.

## 0.22

Improve internal connection handling. The wallet-proxy should now more robustly
detect termination of the connection to the node.

## 0.21

Add support for CIS2TokenMetadata and CIS2TokenBalance queries.

## 0.20

Add support for protocol version 5.

## 0.19

- add cookie forwarding
- introduce new configuration options
  - `--log-level` for controlling log output
  - `grpc-timeout` for controlling the timeout of requests to the node
  - `--secure` which enables TLS support

## 0.18

- add `GET /v0/CIS2Tokens` endpoint.

## 0.17.1

- cover smart contract update transactions in the `v0/transactionCost` endpoint


## 0.17.0
- Support `suggestUrl` in the `appSettings` configuration.
- cover smart contract transactions in the `v0/submissionStatus` endpoint.

## 0.16.0

- add `v1/appSettings` endpoint.

## 0.15.7
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
