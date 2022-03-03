 # wallet-proxy

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

## Overview

The wallet proxy provides the following endpoints:

* `GET /v0/accBalance/{account address}`: get the balance on an account
* `GET /v0/accNonce/{account address}`: get the next nonce for an account
* `GET /v0/accEncryptionKey/{account address}`: get the public encryption key of
  the account
* `GET /v0/transactionCost`: get the cost of a transaction
* `GET /v0/submissionStatus/{transactionHash OR submissionId}`: get the status
  of a transfer or credential deployment
* `PUT /v0/submitCredential`: deploy a credential/create an account
* `PUT /v0/submitTransfer`: perform a simple transfer
* `GET /v0/accTransactions/{accountNumber}`: get the transactions affecting an account
* `GET /v1/accTransactions/{accountNumber}`: get the transactions affecting an account, including memos
* `PUT /v0/testnetGTUDrop/{accountNumber}`: request a CCD drop to the specified account
* `GET /v0/health`: get a response specifying if the wallet proxy is up to date
* `GET /v0/global`: get the cryptographic parameters obtained from the node it is connected to
* `GET /v0/ip_info`: get the identity providers information, including links for
  submitting initial identity issuance requests.

### Errors

In case of a non 200 return code the return value will always be a JSON object with two fields
```json
{
  "errorMessage": "free form string",
  "error": Int
}
```

Where the error codes currently returned are
- 0 for internal server error, most likely the server could not communicate with the node:
  ```json
  {"error":0,"errorMessage":"Error accessing the GRPC endpoint"}
  ```
- 1 for when the request is invalid. There can be a number of reasons for this, but most of them
  should not occur once the initial debugging is over. They indicate that data is malformed in one
  way or another. The message will usually pinpoint the cause of the error.
  ```json
  {"error":1,"errorMessage":"Malformed transaction hash."}
  ```
- 2 for when the request is well-formed, but the requested object could not be
  found, e.g., the account does not exist in the expected place.

### Language & localization

Error messages and textual descriptions will be localized by the proxy based on the `Accept-Language` header.
(Currently, only `en` is available.)
Languages can also be specified with the `_LANG` GET-parameter and/or a `_LANG` cookie; these have higher precedence than `Accept-Language`.

## Identity provider information

The data served on the `v0/ip_info` endpoint is a JSON array of objects. Each
object is of the form
```json
{
    "ipInfo": {...},
    "arsInfos": {...},
    "metadata": {
        "issuanceStart": URL,
        "icon": "base64 encoded png image"
    }
}
```
All fields are mandatory.
The "ipInfo" and "arsInfos" objects are needed when creating identity object requests, and identity objects.

## Account Balance

The balance on an account can be queried as in the following example:

```console
$ curl -XGET localhost:3000/v0/accBalance/4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8
{"currentBalance":AccountBalance,"finalizedBalance":AccountBalance}
```

The result is a JSON object with two __optional__ fields:
- `currentBalance` is the balance on the account in the current best block;
- `finalizedBalance` is the balance on the account in the most recently finalized block.

If neither field is present, then the account does not currently exist on the chain.
If only `currentBalance` is present, then the account has been created, but its creation has not yet been finalized.
Otherwise, both fields will appear.

The `AccountBalance` value is always an object with the following four fields
* `"accountAmount"` (required) which is an Amount type, i.e., a string containing an integral value.
  This amount represents the total amount owned by the account, no matter if it is locked up or not.
  It does not however include the encrypted amounts. To compute the available amount we have to calculate:
  `accountAmount - lockedUpBalance` (from the last point of this list).
* `"accountEncryptedAmount"` (required) which is an object with three (mandatory) fields
  - `"selfAmount"` of type EncryptedAmount, i.e., a hexadecimal string
  - `"startIndex"` a non-negative 64-bit integer
  - `"numAggregated"` (optional) a number >= 2 indicating how many amounts
    are aggregated together in the first amount (see section at the end for an
    explanation). If not present the first amount in the list is a pure incoming amount.
  - `"incomingAmounts"` an array of `EncryptedAmount` values, i.e., an array of
    hexadecimal strings. The array could be empty, but is always present.
* `"accountNonce"` the nonce of the account matching the balance. This is the
  nonce of the next transaction that is not yet included in the balance.
* `"accountReleaseSchedule"` (required) the release schedule for this account consisting of:
  - `"schedule"`: a list of objects with the following fields, all required
    * `"timestamp"` (in milliseconds since Unix Epoch)
    * `"amount"` .. the amount that will be released at the given timestamp
    * `"transactions"` .. an array of transaction hashes that contribute to
      the amount that will be released.
  
  - `"total"`: The sum of all the pending amounts, to be used when calculating the available amount.
  More explicitly, the format of `"accountReleaseSchedule"` is:
  ```
    { "schedule": [
        [ timestamp1, [ amount1, [ txHash11, txHash12...] ] ],
        [ timestamp2, [ amount2, [ txHash21...] ] ],
        ...
        ],
      "total" : totalamount
    }
  ```
* `"accountBaker"` (optional) if present indicates that this account is
 registered as a baker. If present, the value is always an object with fields
  - `"stakedAmount"` (required): the amount that is currently staked
  - `"bakerId"` (required): the baker id the account is registered as
  - `"restakeEarnings"` (required): a boolean indicating whether earnings are
    added to stake.
  - `"bakerAggregationVerifyKey"` (required): public key to verify aggregate signatures in which the baker participates.
  - `"bakerElectionVerifyKey"` (required): public key to verify the baker has won the election.
  - `"bakerSignatureVerifyKey"` (required): public key to verify block signatures signed by the baker.

## Account Nonce

When making a transfer the wallet must select a nonce for the transaction. This nonce must be sequential.
The server provides a "best effort" functionality for querying the nonce. This is on the endpoint `accNonce`
which will always return a JSON object (unless there is an internal server error) consisting of two fields
```json
{
  "nonce": UInt,
  "allFinal": Bool
}
```

Example:
```console
$ curl -XGET localhost:3000/v0/accNonce/4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8
{"allFinal":true,"nonce":3}
```

The `nonce` is always the next nonce that should be used provided all the known transactions will be finalized eventually.
- If `allFinal` is `True` then all transactions from this account are finalized and the nonce should be considered reliable.
- Otherwise there are some pending transactions so the nonce returned is a best guess assuming all transactions will be successful.

In case the wallet is the only user of the account then this nonce tracking is reliable.
If there are other concurrent users of the account then there is a chance the
nonce returned will be wrong, but then it is up to the user to keep track of that themselves.

## Account Encryption key

Returns the public key of the account that can be used when making encrypted
transfers. If the request is invalid, i.e., malformed address, the status code
in 500+ range is returned, if the request is valid, but the account does not
exist, a status code 404 is returned. In both these cases the returned object is
of the form of a generic error object, described above.

In case of success, the return code is 200, and the return value is an object of
the form

```json
{
  "accountEncryptionKey": String,
}
```
The field is mandatory, and the value will always be a hex-encoded public key.

## Transaction cost.

The cost for a transaction, both in energy and CCD is obtained on the
`v0/transactionCost` endpoint.
The following query parameters are supported
- `type`, the type of the transaction. This is mandatory and can be one of `simpleTransfer`, `encryptedTransfer`, `transferToSecret` or `transferToPublic`.
- `numSignatures`, the number of signatures on the transaction, defaults to 1 if not present.
- `memoSize`, the size of the transfer memo. Optionaly, and only supported if the node is running protocol version 2 or higher, and only applies when `type` is either `simpleTransfer` and `encryptedTransfer`.

In case of success the response will always be a JSON object with required fields
- `"cost"` which is an Amount of CCD this transfer will cost at current
  conversion rates
- `"energy"` which is the energy that is required be supplied for execution of this transaction.

In case of invalid parameters the response will be as described in the [errors section](#errors) with the following possible status codes
- `400` if any of the following apply
  - the transaction type parameter is missing
  - numSignatures is present but it cannot be parsed as an integer
  - memoSize is present but it cannot be parsed as an integer
- `404` if `memoSize` is present, but the node that backs the wallet-proxy is still running protocol version 1.
- `502` if the wallet-proxy cannot access the node.

## Submission Status

A GET request to `/submissionStatus/{transactionHash OR submissionId}` returns a
JSON object summarizing the status of a transfer or credential deployment.
On success, the response is of the following form:
```
{
  "status": "finalized",
  "amount": "20000",
  "sender": "4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8",
  "to": "3J6vgTViNgjc4gxSgTkZWa2aspuitVCRrkkaTqQjFXHnkENaSk",
  "cost": 165,
  "transactionHash": "52277a488216a8914bf3d575a644a98b5592b62da2b91a45ca16302478e0583a",
  "outcome": "success",
  "blockHashes": [
    "b8b24624ca089a31a63afd5934a666c7a8e198c1e18970721ff0cf8b606fc16d"
  ]
}
```
Only the `status` field is required.
Other fields are present depending on the value of the `status` field and the type and result of the transaction.

#### `status` (required)
One of the following values:
* `"absent"`: the transaction is not known to the node
* `"received"`: the transaction has been received but not yet added to any block
* `"committed"`: the transaction is present in one or more blocks, but not a finalized block
* `"finalized"`: the transaction is present in a finalized block

If a submission is finalized it will always stay finalized. A received
transaction can transition into either committed, directly to finalized, or to
absent. A transaction that is committed is most likely going to transition to a
finalized one, although it is technically possible that it will become absent as well.

#### `outcome` (optional)
This field is present if the `status` field is `committed` or `finalized`.
The possible values are:
* `"success"`: the transaction completed successfully
* `"reject"`: the transaction failed
* `"ambiguous"`: the transaction has different outcomes in different blocks

The `ambiguous` outcome only applies to `committed` transactions.

#### `rejectReason` (optional)
This field is present if `outcome` is `reject`.
It contains a description of the reason for rejection.

#### `transactionHash` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is the hash of the transaction.

#### `sender` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is either the account address of the sender for a simple transfer, or `null` for a credential deployment.

#### `cost` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is a number representing the actual cost of the transaction to the sender.
(The value is an integer in the smallest denomination of CCD.)

#### `to` (optional)
This field is present if the `status` field is `committed` or `finalized`, the `outcome` field is `success`, and the transaction is a simple or encrypted transfer.
The value is the account address of the recipient of the transfer.

#### `amount` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `success`, and the transaction is a simple transfer.
The value is a number representing the amount transferred from the sender to the recipient.
(The value is an integer in the smallest denomination of CCD.)

#### `blockHashes`
This field is present if the `status` field is `committed` or `finalized`.
The value is a (non-empty) array of the hashes of all (live) blocks in which the transaction appears.
If the `status` is `finalized`, the array will only have one element.

#### `newSelfEncryptedAmount` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `success`, and the transaction is one of the three encrypted transactions. The value is the new self encrypted amount on the account.

#### `inputEncryptedAmount` (optional)
This field is present if the `status` field is `committed` or `finalized`, and
the outcome field is `success`, and the transaction is either an encrypted
transfer, or encrypted to public transfer, i.e., unshielding. The value is the
input encrypted amount that was removed from the sender's account.

#### `aggregatedIndex` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `success`, and the transaction is either `EncryptedAmountTransfer` or `TransferToPublic`. The value is the index up to which the self encrypted amounts have been combined during the operation that was performed.

#### `amountAdded`/`amountSubtracted` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `success`. For a `TransferToPublic`, the field will be named `amountAdded` and it represents the plaintext amount that is added to the public balance of the sender. For a `TransferToEncrypted`, the field will be named `amountSubtracted` and it represents the plaintext amount that is subtracted from the public balance of the sender.

## Credential deployment/account creation

Using either the example credential in [examples/cdi.json](examples/cdi.json) or
the initial account creation transaction in [examples/initial-account.json](examples/initial-account.json)
the following displays an interaction that leads to credential deployment.

```console
$ curl -XPUT -d "@examples/cdi.json" localhost:3000/v0/submitCredential
{
  "submissionId": "af8d639191fa6194c0ff285bcedb1f47b8b05ca477aae68dae26e1c524e06888"
}
```

If the credential is well-formed and communication with the server is good, and
the node is alive the server will respond with a submission id which can be
queried via the `/submissionStatus` endpoint.


## Submit transfer

When submitting a transfer you should make a PUT request to `/v0/submitTransfer` endpoint.
The data that should be sent is as the one returned from the library provided as part of the concordium-base repository.
After submission of the transaction the responses are the same as for the submission of the credential. If successful
a submission id is returned, which can be used to query the status of the transfer via the `/v0/submissionStatus` endpoint.

## Get transactions

The endpoint `/accTransactions/{account address}` retrieves a partial list of transactions affecting an account.
There are two versions of the endpoint, `v0` and `v1`.
They both support the following parameters.
- `order`: whether to order the transactions in ascending or descending order of occurrence. A value beginning with `d` or `D` is interpreted as descending; any other (or no) value is interpreted as ascending.
- `from`: a transaction id. If the order is ascending, return transactions with higher ids than this; if the order is descending, return transactions with lower ids.
- `limit`: the maximum number of transactions to return; defaults to 20; values above 1000 are treated as 1000.
- `includeRewards`: whether to include rewards, and if so, which ones. This is
  an optional parameter which defaults to including all rewards. The possible
  values are
  - `none`: include no rewards, including minting
  - `allButFinalization`: include all but finalization rewards
  - `all`: include all rewards. This is also the default if not supplied.
- `blockTimeFrom`: exclude any transactions with block time earlier than `blockTimeFrom` (integer number of seconds after epoch).
- `blockTimeTo`: exclude any transactions with block time later than  `blockTimeTo` (integer number of seconds after epoch).
- `blockRewards`: whether to include block rewards. Possible values:
  - `y`: include block rewards. (The default)
  - `n`: exclude block rewards.
- `finalizationRewards`: whether to include finalization rewards. Possible values:
  - `y`: include finalization rewards. (The default)
  - `n`: exclude finalization rewards.
- `bakingRewards`: whether to include baking rewards. Possible values:
  - `y`: include baking rewards. (The default)
  - `n`: exclude baking rewards.
- `onlyEncrypted`: whether to only include transactions related to encrypted amounts (i.e. shield, unshield, and transfer shielded). Possible values:
  - `n`: include all transactions. (The default)
  - `y`: only include shield, unshield, and transfer shielded transactions.
- `includeRawRejectReason`: whether to include the raw rejection reason (the
  same JSON as returned by `GetTransactionStatus` gRPC endpoint).

The result is a JSON object with the following fields:
- `order`: either `"ascending"` or `"descending"` indicating the ordering applied to the transactions.
- `from`: the transaction id offset, if one was specified.
- `limit`: the limit on the number of transactions returned.
- `count`: the actual number of transactions returned. This can be less than limit if there are no more transactions to return.
- `transactions`: an array of transactions in the order specified.

A transaction consists of the following fields:

#### `id` (required)

This is a stable, ordered identifier for a finalized transaction.
It is a positive integer.
Transactions with higher ids always happen later.

#### `origin` (required)

The originator of the transaction.
This is a JSON object with the following fields:
- `type`:
  - `"self"` if the transaction originates from the account itself
  - `"account"` if the transaction originates from another account
  - `"reward"` if the transaction originates as a reward (e.g. for baking)
  - `"none"` if the transaction has no originator (e.g. a credential deployment)
- `address`: an account address; present only if `type` is `"account"`.

#### `blockHash` (required)
The hash of the block in which the transaction occurs.

#### `blockTime` (required)
The nominal time at which the block was baked.
The time is given in seconds since the UNIX epoch, given as a fractional number,
i.e., floating point number. Note that on JSON if a float number has no decimals, it is
outputted as an Int instead of having `.00` or something like that.

#### `transactionHash` (optional)
This is the hash of the transaction.
This is not present for special transactions, such as rewards.

#### `subtotal` (optional)
The change in the account's __public__ balance due to this transaction, not including the transaction fee.
This is only present if the origin type is `"self"` and the transaction involves a transfer to or from the account other than the transaction fee.

#### `encrypted` (optional)
The change in the encrypted balance of the account. This is only present if the
encrypted balance was changed.
- If the `origin` field is `account` then this field is an object with required fields
  - `"encryptedAmount"` which is always a hexadecimal encoding of an encrypted
    amount, i.e., it is a string.
  - `"newIndex"` which is always a non-negative integer
- If the `origin` field is `self` then this field is an object with
  fields
  - `"newSelfEncryptedAmount"` (required) which is always a hexadecimal encoding of an encrypted
    amount, i.e., it is a string.
  - `"newStartIndex"` (optional) which, if present, is a non-negative integer
    that indicates which encrypted amounts were used in this transfer. This is
    only present if the transaction type is an encrypted amount transfer, or a
    transfer from secret to public balance.
  - `"encryptedAmount"` (optional) in case of encrypted transfer, the amount
    that was sent. In public to secret and secret to public transfers this field
    will not be present.

#### `cost` (optional)
The cost of performing the transaction, if this cost is paid by the account.
This is only present if the origin type is `"self"` (since otherwise the account is not responsible for the cost).
When present, this is always a positive value.

#### `total` (required)
The total change in the account's __public__ balance due to the transaction and associated fees.
A negative value indicates a debit from the account, while a positive value indicates a credit to the account.

#### `energy` (optional)
The energy cost (in NRG) of executing the transaction.
This is not present for special transactions.

#### `details` (required)
A JSON object containing more details about the transaction.
It consists of the following fields:

- `type` (required): in `v0` endpoint one of the following:
  - `"deployModule"`
  - `"initContract"`
  - `"update"`
  - `"transfer"`
  - `"addBaker"`
  - `"removeBaker"`
  - `"updateBakerStake"`
  - `"updateBakerKeys"`
  - `"updateBakerRestakeEarnings"`
  - `"encryptedAmountTransfer"`
  - `"transferToEncrypted"`
  - `"transferToPublic"`
  - `"transferWithSchedule"`
  - `"registerData"`
  - `"bakingReward"`
  - `"platformDevelopmentCharge"` (only returned when viewing the foundation account)
  - `"finalizationReward"`
  - `"blockReward"`
  - `"paydayFoundationReward"`
  - `"paydayAccountReward"`
  - `"blockAccrueReward"` (in practice, this is not possible, as the transaction does not pertain to any particular account)
  - `"paydayPoolReward"` (in practice, this is not possible, as the transaction does not pertain to any particular account)
  - `"deployCredential"`
  - `"updateCredentials"`
  - `"updateAccountKeys"`
  - `"chainUpdate"`
  - `"Malformed account transaction"`
  - in `v1` endpoint type can additionally be one of
    - `transferWithMemo`
    - `encryptedAmountTransferWithMemo`
    - `transferWithScheduleAndMemo`
- `description` (required): a brief localized description of the type of the transaction
- `outcome` (required):
  - `"success"` if the transaction was executed successfully
  - `"reject"` if the transaction failed, in which case it had no effect other than charging the fee
- `rejectReason` (when outcome is `"reject"`): a brief localized description of why the transaction was rejected
- `events` (when outcome is `"success"`): an array of strings consisting of brief localized descriptions of the on-chain events resulting from the transaction
- The following fields are present if the transaction is a simple transfer or a transfer with memo:
  - `transferSource`: account address of the source of the transfer
  - `transferDestination`: account address of the destination of the transfer
  - `transferAmount`: amount of the transfer
  - in `v1` version if `type = transferWithMemo` then an additional field `memo` is present. It is a hex-encoded byte array.
- The following fields are present if the transaction is an encrypted amount transfer or an encrypted amount transfer with memo:
  - `transferSource`: account address of the source of the transfer
  - `transferDestination`: account address of the destination of the transfer
  - `encryptedAmount`: the encrypted amount that is transferred in the transaction in hexadecimal encoding.
  - `inputEncryptedAmount`: the encrypted amount that was used by the sender as input to the transaction, i.e., consumed
  - `aggregatedIndex`: the index up to which incoming amounts on the sender account have been combined during this operation.
  - `newIndex`: the index on the receiver's incomingAmounts that will be assigned to the transferred amount.
  - `newSelfEncryptedAmount`: the resulting self encrypted amount in the sender's account.
  - in `v1` version if `type = encryptedAmountTransferWithMemo` then an additional field `memo` is present. It is a hex-encoded byte array.
- The following fields are present if the transaction is a transfer to public transaction:
  - `transferSource`: account address of the source of the transfer
  - `amountAdded`: the plaintext of the amount that is added to the sender's public balance.
  - `aggregatedIndex`: the index up to which incoming amounts on the sender account have been combined during this operation.
  - `newSelfEncryptedAmount`: the resulting self encrypted amount in the sender's account.
  - `inputEncryptedAmount`: the encrypted amount that was used by the sender as input to the transaction, i.e., consumed
- The following fields are present if the transaction is a transfer to encrypted transaction:
  - `transferSource`: account address of the source of the transfer
  - `amountSubtracted`: the plaintext of the amount that is subtracted from the sender's public balance.
  - `newSelfEncryptedAmount`: the resulting self encrypted amount in the sender's account.
- The following fields are present if the transaction is a transfer with schedule or transfer with schedule and memo:
  - `transferDestination`: account address of the destination of the transfer
  - `transferAmount`: amount of the transfer
  - in `v1` version if `type = transferWithScheduleAndMemo` then an additional field `memo` is present. It is a hex-encoded byte array.
- The following field is present if the transaction is a register data transaction:
  - `registeredData`: hex encoding of the data that was registered

For the purposes of the above, a simple transfer is a transaction of type `"transfer"` which transfers funds from one account to another.
A transactions of type `"transfer"` is not considered a simple transfer if the destination is a smart contract instance.
### Example

```console
$ curl -XGET http://localhost:3000/v0/accTransactions/4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL?limit=2&from=4&order=a
```
```JSON
{
  "transactions": [
    {
      "id": 5,
      "blockTime": 1587646256.25,
      "origin": {
        "type": "self"
      },
      "energy": 59,
      "cost": 59,
      "subtotal": 0,
      "transactionHash": "84bf1e2ef8d3af3063cdb681932990f71ddb3949655f55307a266e5d687b414f",
      "blockHash": "013c6d2dd67affd6f39b9a7b255d244055b53d68fe8b0add4839a20e911d04cb",
      "details": {
        "transferAmount": "123",
        "events": [
          "Transferred 123 from 4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL to 4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL"
        ],
        "outcome": "success",
        "type": "transfer",
        "transferDestination": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
        "description": "Transfer",
        "transferSource": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL"
      },
      "total": -59
    },
    {
      "id": 7,
      "blockTime": 1587646300.02,
      "origin": {
        "type": "reward"
      },
      "blockHash": "7a496e01bf67ad4fe720551185cf10c05acd8d4c91e995826d0703193eeac2b4",
      "details": {
        "events": [
          "Award 6270 to baker 0 at 4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL"
        ],
        "outcome": "success",
        "type": "bakingReward",
        "description": "Baking reward for baker 0"
      },
      "total": 6270
    }
  ],
  "from": 4,
  "count": 2,
  "limit": 2,
  "order": "ascending"
}
```

## Testnet CCD Drop

Request a CCD drop to the specified account.
On success, this returns a JSON object with the field `submissionId`, which contains a transaction hash that may be used to query the status of the drop transaction via the `/submissionStatus` endpoint.


```console
$ curl -XPUT http://localhost:3000/v0/testnetGTUDrop/3KudzzbqRPyHVFEzZnZCUdK2ixr1SFLRVRm6sTfEvQv2N3A8h6
{"submissionId":"76031716c829f3e7e95efda77558631d348a483e3317e3d66f3cac3038e5e757"}
```

The operation is idempotent: only one drop will be issued per account.
If for some reason the drop fails, subsequent calls could return a new `submissionId`.
(This is unlikely under normal circumstances, but could result from racing concurrent requests.)

If the account address is well-formed, but the account does not exist in a finalized state on the chain, this call fails with a **404 Not found** status code.


## Health

Returns an object specifying if the information accessible from the wallet proxy is up to date.
It will query the GRPC and the transaction database. Assuming both succeed it checks that the last final block is less than `health-tolerance` seconds old.
`health-tolerance` is an optional parameter to the wallet-proxy. It defaults to 300 seconds if left unspecified.

Under normal conditions the health query returns:
```json
{
  "healthy":true,
  "lastFinalTime":"2021-07-06T11:55:49.5Z"
}
```
If the queries succeeded but the last final block is too old it will return:
```json
{
  "healthy":false,
  "reason":"The last final block is too old.",
  "lastFinalTime":"2021-07-06T11:55:49.5Z"
}
```
If one of the queries fail, it could return `healthy=false` with a reason, or an error.

## Notes on account balances.

Suppose that at time t₀ you query the account balance and get a structure
```json
{
   "selfAmount": s₀
   "startIndex": i₀
   "numAggregated": n₀
   "incomingAmounts": a_{i₀} .. a_{iₙ}
}
```

This implies that the amount `a_{i₀}` is not an incoming amount directly, i.e.,
it is not on any of the incoming transactions, but it is an aggregate of
incoming amounts `i₀ + 1 - n₀`, ..., up to `i₀`. All of the rest of the amounts
are pure incoming amounts, as sent by other accounts.

Next time we query we get the account balance like

```json
{
   "selfAmount": t₀
   "startIndex": j₀
   "numAggregated": m₀
   "incomingAmounts": b_{j₀} .. b_{jₙ}
}
```

The first amounts in the respective lists are aggregation of `n₀` (respectively
`m₀`) amounts. They are amounts with indices `a_{i₀+1-m},..,a_{i₀}`

### Case where `selfAmounts` are the same

The only change then would have been the arrival of new incoming encrypted
amounts. In this case then `j₀ ≥ i₀` and `m₀ ≥ i₀`.

The amount `b_{j₀}` might not yet be seen, however we know that it is the
aggregation of amounts with indices `b_{j₀+1-m},..,a_{j₀}`, which we already
have (partially) decrypted, and then we can count those towards the public
balance already, while continuing to decrypt the remaining ones.

### Case when `selfAmount` are different

In this case the wallet must have made some action to cause this (or the user
has used account externally), in both of these cases you can do a similar
analysis with indices to determine the partial balance that you have already
decrypted, but of course in this case it will change.

# Deployment

The wallet proxy must have access to
- a running node via its GRPC interface
- a transaction outcome database, a postgres database
- when it starts it needs to be given a file with the list of identity providers
  and anonymity revokers, and their public keys and metadata.
- optionally, private keys of the CCD drop account. If this is not provided then
  the gtu drop functionality will be disabled.

An example invocation is
```console
wallet-proxy --grpc-ip 127.0.0.1\ # IP of the node
             --grpc-port 10000\ # GRPC port the node is listening on
             --db "host=localhost port=5432 dbname=transaction-outcome user=postgres password=postgres"\ # transaction outcome database connection string
             --ip-data identity-providers-with-metadata.json\ # JSON file with identity providers and anonymity revokers
             --drop-account gtu-drop-account-0.json # keys of the gtu drop account
             --health-tolerance 30 # tolerated age of last final block in seconds before the health query returns false
```

## Identity providers metadata file

This must be a valid JSON file which contains an array of JSON objects of the following form
```json
{
  "metadata": {
    "issuanceStart": "https://identity.provider/issuance-start",
    "icon": "base 64 encoded png image",
    "support": "<support@identity-provider.id>"
  },
  "ipInfo": {
    "ipIdentity": 0,
    "ipDescription": {
      "name": "Short name as it appears on the chain.",
      "url": "http/identity.provider",
      "description": "Free form description"
    },
    "ipVerifyKey": "...",
    "ipCdiVerifyKey": "74e905294a9377408d87ab4ddc4202731c4f971561eeaf423e82ae9509b8d057"
  },
  "arsInfos": {
    "1": {
      "arIdentity": 1,
      "arDescription": {
        "name": "AR-1",
        "url": "",
        "description": ""
      },
      "arPublicKey": 93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201886a215138ec53d870e2878bbe731381927e08eaafe97003f6f4831f18e47c9ee8913c5f806064b57341785f0376af"
    },
    "2": {
      "arIdentity": 2,
      "arDescription": {
        "name": "AR-2",
        "url": "",
        "description": ""
      },
      "arPublicKey": 93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201ac7295a21c3c687112f454c1d222d74e0d9cc9249b3c1eef58eb66a8a039c0decf3ea413a656f6f2dbebb497b7a527"
    }
  }
}
```

Where
- the `ipInfo` field is the contents of the `identity-provider-*.pub.json` files generated by the genesis tool, minus the outer versioning.
- the `arsInfos` field has the same format (minus the versioning) as the `anonymity_revokers.json` file generated by the genesis tool.
- the `metadata` field needs to be constructed manually based on the desired setup and in communication with partners.
  - the `issuanceStart` link is where the wallet submits the initial identity creation request.
  - the `icon` needs to be a base64 encoded png image that should be obtained from the relevant identity provider.
  - the `support` field must contain a valid support email of the identity provider.

NB: It is OK to have the same identity provider listed multiple times in this file, i.e., the same identity provider could have two verification backends, in which case they would be listed twice in the list, the difference between the two instances being the `issuanceStart` and `icon` fields.

## Database setup

The wallet-proxy needs access to the transaction logging database in the form of a PostgreSQL database.
It assumes the layout and semantics is as described in the [transaction logging notes](https://github.com/Concordium/concordium-node/blob/main/docs/transaction-logging.md).
The wallet-proxy queries the database in specific patterns. Every query filters by a single account/contract and narrows down by id.
Some queries additionally filter out per transaction type or by time.
To support efficient retrieval in the common cases it is necessary that the `ati` and `cti` tables have a primary key index on the joint `(account, id)` columns (and analogous columns `(index, subindex, id)` for the `cti` table).
The order of columns matters, since PostgreSQL will make best use of these indices if the queries have an equality constraint on the leading columns and a relational constraint on the remaining ones.
