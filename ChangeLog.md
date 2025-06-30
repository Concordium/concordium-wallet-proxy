# Changelog for wallet-proxy

## Unreleased changes

## [0.38.0] - 2025-06-30

- Rejected PLT txs are parsed in the `GET /v3/accTransactions/{account address}` response.
- PLT transaction types have been consolidated to only one type `tokenUpdate` (previously `tokenGovernance`/`tokenHolder` types existed).

## [0.37.0] - 2025-06-25

- Add endpoint `GET /v3/accTransactions/{account address}` that gets the transactions affecting an account's ccd or plt balances (including special transaction outcomes for suspended/inactive validators and including CCD transactions with memos).
- Add endpoint `GET /v2/accBalance/{account address}` that includes the plt (protocol level token) balances of an account.
- Add endpoint `GET /v0/plt/tokens` to get a list of all plt token infos.
- Add endpoint `GET /v0/plt/tokenInfo/{tokenId}` to get the token info about a given plt token and its module state (with metadata url)
- Add flag to provide a server port at run-time.

## 0.36.0

- Add `PUT /v0/submitRawTransaction` endpoint for submitting an account transaction as bytes instead of JSON.

## 0.35.0

- Add `isPrimedForSuspension` and `isSuspended` to `accBalance` queries where the account is
  a delegator or validator.
- Add the parameter `suspended` for `/v0/transactionCost?type=configureBaker` to calculate the
  cost when updated the suspended status of a validator.

## 0.34.2

- Fix a bug where account transactions where incorrectly excluded from the
  `/v0/accTransactions` and `/v1/accTransactions` endpoints.

## 0.34.1

- Fix a bug in the deserialization of `SpecialTransactionOutcome` that causes the
  `/v2/accTransactions` endpoint to fail with `validatorPrimedForSuspension` and
  `validatorSuspended` events.

## 0.34.0

- `/v*/accBalance` endpoints include `isSuspended` flag in the `bakerPoolInfo`.
- `/v0/bakerPool` endpoint includes `isSuspended`, `isPrimedForSuspension` and `missedRounds`.
- Add `/v2/accTransactions` endpoint that includes `validatorPrimedForSuspension` and
  `validatorSuspended` pseudo-transaction types.

## 0.33.0

- Update GHC version to 9.6.6 (lts-22.39).
- Support timestamps in scheduled transactions serialized in RFC3339 format in the database.

## 0.32.1

- Endpoint `/v1/accBalance` displays correct `accountAtDisposal` when used with a node version < 7.

## 0.32.0

- Add endpoint `/v1/accBalance` that exposes the cooldowns on the account
  (`accountCooldowns`) and the available account balance (`accountAtDisposal`).
- Revise `/v0/bakerPool` so that pools that have been closed are treated as
  not found.

## 0.31.0

- Introduce `/v2/ip_info` endpoint which includes Company ID providers. The information is provided to the service using `--ip-data-v2 <FILE>`.

## 0.30.1

- Reduce the amount of set-cookie headers set in complex queries.

## 0.30.0

- Add endpoints `v1/CIS2TokenMetadata` and `v1/CIS2TokenBalance` that return a
  subset of tokens which could be successfully queried.
- Update GHC version to 9.6.4 (lts-22.9).

## 0.29.2

- Rename baker to validator in transaction outcomes.

## 0.29.1

- Improve reconnect handling. If the request to the node times out or fails due
  to resource exhaustion then the connection to the node is longer reset.

## 0.29.0

- Add an optional `success` field to the `transactionStatus` response if
  querying the cost of a contract update. It indicates whether contract
  execution succeeded.

## 0.28.0

- Support node version 6.

## 0.27.0

- Make the CCD drop amount configurable at runtime via the `--drop-amount` option.

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
