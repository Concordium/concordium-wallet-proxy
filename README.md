# wallet-proxy

## Overview

The wallet proxy provides the following endpoints:

* `GET /v0/accBalance/{account address}`: get the balance on an account
* `GET /v0/accNonce/{account address}`: get the next nonce for an account
* `GET /v0/accEncryptionKey/{account address}`: get the public encryption key of
  the account
* `GET /v0/simpleTransferCost`: get the cost of a simple transfer
* `GET /v0/submissionStatus/{submissionId}`: get the status of a simple transfer or credential deployment
* `PUT /v0/submitCredential`: deploy a credential/create an account
* `PUT /v0/submitTransfer`: perform a simple transfer
* `GET /v0/accTransactions/{accountNumer}`: get the transactions affecting an account
* `PUT /v0/testnetGTUDrop/{accountNumber}`: request a GTU drop to the specified account
* `GET /v0/global`: get the global parameters (serves the file [`global.json`](https://gitlab.com/Concordium/genesis-data/-/blob/master/global.json)).
* `GET /v0/ip_info`: get the identity providers info (serves the file [`identity_providers_with_metadata.json`](https://gitlab.com/Concordium/genesis-data/-/blob/master/identity-providers-with-metadata.json)).

### Errors

In case of a non 200 return code the return value will always be a JSON object with two fields
```json
{
  "errorMessage": "free form string",
  "error": Int
}
```

Where the error codes currently returned are
- 0 for internal server error, most likely the server could not communicate with the the baker:
  ```json
  {"error":0,"errorMessage":"Error accessing the GRPC endpoint"}
  ```
- 1 for when the request is invalid. There can be a number of reasons for this, but most of them
  should not occur once the initial debugging is over. They indicate that data is malformed in one
  way or another:
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
$ curl -XGET localhost:3000/accBalance/4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8
{"currentBalance":AccountBalance,"finalizedBalance":AccountBalance}
```

The result is a JSON object with two __optional__ fields:
- `currentBalance` is the balance on the account in the current best block;
- `finalizedBalance` is the balance on the account in the most recently finalized block.

If neither field is present, then the account does not currently exist on the chain.
If only `currentBalance` is present, then the account has been created, but its creation has not yet been finalized.
Otherwise, both fields will appear.

The `AccountBalance` value is always an object with the following two fields
both of which will be present
* `"accountAmount"` which is an Amount type, i.e., a string containing an integral value.
* `"accountEncryptedAmount"` which is an object with three (mandatory) fields
  - `"selfAmount"` of type EncryptedAmount, i.e., a hexadecimal string
  - `"startIndex"` a non-negative 64-bit integer
  - `"incomingAmounts"` an array of `EncryptedAmount` values, i.e., an array of
    hexadecimal strings. The array could be empty, but is always present.

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
$ curl -XGET localhost:3000/accNonce/4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8
{"allFinal":true,"nonce":3}
```

The `nonce` is always the next nonce that should be used provided all the known transactions will be finalized eventually.
- If `allFinal` is `True` then all transactions from this account are finlized and the nonce should be considered reliable.
- Otherwise there are some pending transactions so the nonce returned is a best guess assuming all transctions will be successful.

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

## Simple Transfer Cost

The cost for a simple transfer using one signature can be obtained as follows:

```console
$ curl -XGET localhost:3000/simpleTransferCost
{"cost":59}
```

The result is a JSON object with one field `cost` that gives the current estimated cost of such a transaction in the smallest denomination of GTU.


## Submission Status

A GET request to `/submissionStatus/[transactionHash]` returns a JSON object summarizing the status of a simple transfer or credential deployment.
On success, the response is of the following form:
```
{
  "status": "finalized",
  "amount": 20000,
  "sender": "4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8",
  "to": "3J6vgTViNgjc4gxSgTkZWa2aspuitVCRrkkaTqQjFXHnkENaSk",
  "cost": 165,
  "transactionHash": "52277a488216a8914bf3d575a644a98b5592b62da2b91a45ca16302478e0583a",
  "outcome": "transferSuccess",
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
It contains a description of of the reason for rejection.

#### `transactionHash` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is the hash of the transaction.

#### `sender` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is either the account address of the sender for a simple transfer, or `null` for a credential deployment.

#### `cost` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is a number representing the actual cost of the transaction to the sender.
(The value is an integer in the smallest denomination of GTU.)

#### `to` (optional)
This field is present if the `status` field is `committed` or `finalized`, the `outcome` field is `success`, and the transaction is a simple transfer.
The value is the account address of the recipient of the simple transfer.

#### `amount` (optional)
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `success`, and the transaction is a simple transfer.
The value is a number representing the amount transferred from the sender to the recipient.
(The value is an integer in the smallest denomination of GTU.)

#### `blockHashes`
This field is present if the `status` field is `committed` or `finalized`.
The value is a (non-empty) array of the hashes of all (live) blocks in which the transaction appears.
If the `status` is `finalized`, the array will only have one element.


## Credential deployment/account creation

Using the example credential in [examples/cdi.json](examples/cdi.json) the
following displays an interaction that leads to credential deployment.

```console
$ curl -XPUT -d "@examples/cdi.json" localhost:3000/submitCredential
{
  "submissionId": "f1a3a3c17e3bc70a4dad3f409265f8a1fca07c607b732e11cf279dd2e891e0af"
}
```

If the credential is well-formed and communication with the server is good, and
the baker node is alive the server will respond with a submission id which can
be queried via the `/submissionStatus` endpoint.


## Simple transfer

When submitting a simple transfer you should make a PUT request to `/submitTransfer` endpoint.
The data that should be sent is as the one returned from the library provided as part of the crypto repository.
After submission of the transaction the responses are the same as for the submission of the credential. If successful
a submission id is returned, which can be used to query the status of the transfer via the `/submissionStatus` endpoint.


## Get transactions

The endpoint `/accTransactions/{account address}` retrieves a partial list of transactions affecting an account.
The following parameters are supported:
- `order`: whether to order the transactions in ascending or descending order of occurrence. A value beginning with `d` or `D` is interpreted as descending; any other (or no) value is interpreted as ascending.
- `from`: a transaction id. If the order is ascending, return transactions with higher ids than this; if the order is descending, return transactions with lower ids.
- `limit`: the maximum number of transactions to return; defaults to 20; values above 1000 are treated as 1000.

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
The time is given in seconds since the UNIX epoch.
(Currently, it is always an integer, although in future this may have a sub-second resolution.)

#### `transactionHash` (optional)
This is the hash of the transaction.
This is not present for special transactions, such as rewards.

#### `subtotal` (optional)
The change in the account's public balance due to this transaction, not including the transaction fee.
This is only present if the origin type is `"self"` and the transaction involves a transfer to or from the account other than the transaction fee.

### `encrypted` (optional)
The change in the encrypted balance of the account. This is only present if the
encrypted balance was changed. 
- If the `origin` field is `account` then this field is an object with required fields
  - `"encryptedAmount"` which is always a hexadecimal encoding of an encrypted
    amount, i.e., it is a string.
  - `"index"` which is always a non-negative integer
- If the `origin` field is `self` then this field is an object with
  fields
  - `"newSelfBalance"` (required) which is always a hexadecimal encoding of an encrypted
    amount, i.e., it is a string.
  - `"newStartIndex"` (optional) which, if present, is a non-negative integer
    that indicates which encrypted amounts were used in this transfer. This is
    only present if the transaction type is an encrypted amount transfer, or a
    transfer from secret to public balance.

#### `cost` (optional)
The cost of performing the transaction, if this cost is paid by the account.
This is only present if the origin type is `"self"` (since otherwise the account is not responsible for the cost).
When present, this is always a positive value.

#### `total` (required)
The total change in the account's public balance due to the transaction and associated fees.
A negative value indicates a debit from the account, while a positive value indicates a credit to the account.

#### `energy` (optional)
The energy cost (in NRG) of executing the transaction.
This is not present for special transactions.

#### `details` (required)
A JSON object containing more details about the transaction.
It consists of the following fields:

- `type` (required): one of the following:
  - `"deployModule"`
  - `"initContract"`
  - `"update"`
  - `"transfer"`
  - `"addBaker"`
  - `"removeBaker"`
  - `"updateBakerAccount"`
  - `"updateBakerSignKey"`
  - `"delegateStake"`
  - `"undelegateStake"`
  - `"updateElectionDifficulty"`
  - `"deployCredential"`
  - `"bakingReward"`
- `description` (required): a brief localized description of the type of the transaction
- `outcome` (required):
  - `"success"` if the transaction was executed successfully
  - `"reject"` if the transaction failed, in which case it had no effect other than charging the fee
- `rejectReason` (when outcome is `"reject"`): a brief localized description of why the transaction was rejected
- `events` (when outcome is `"success"`): an array of strings consisting of brief localized descriptions of the on-chain events resulting from the transaction
- The following fields are present if the transaction is a simple transfer:
  - `transferSource`: account address of the source of the transfer
  - `transferDestination`: account address of the destination of the transfer
  - `transferAmount`: amount of the transfer

For the purposes of the above, a simple transfer is a transaction of type `"transfer"` which transfers funds from one account to another.
A transactions of type `"transfer"` is not considered a simple transfer if the destination is a smart contract instance.

### Example

```console
$ curl -XGET http://localhost:3000/accTransactions/4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL?limit=2&from=4&order=a
```
```JSON
{
  "transactions": [
    {
      "id": 5,
      "blockTime": 1587646256,
      "origin": {
        "type": "self"
      },
      "energy": 59,
      "cost": 59,
      "subtotal": 0,
      "transactionHash": "84bf1e2ef8d3af3063cdb681932990f71ddb3949655f55307a266e5d687b414f",
      "blockHash": "013c6d2dd67affd6f39b9a7b255d244055b53d68fe8b0add4839a20e911d04cb",
      "details": {
        "transferAmount": 123,
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
      "blockTime": 1587646300,
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

## Testnet GTU Drop

Request a GTU drop to the specified account.
On success, this returns a JSON object with the field `submissionId`, which contains a transaction hash that may be used to query the status of the drop transaction via the `/submissionStatus` endpoint.


```console
$ curl -XPUT http://localhost:3000/testnetGTUDrop/3KudzzbqRPyHVFEzZnZCUdK2ixr1SFLRVRm6sTfEvQv2N3A8h6
{"submissionId":"76031716c829f3e7e95efda77558631d348a483e3317e3d66f3cac3038e5e757"}
```

The operation is idempotent: only one drop will be issued per account.
If for some reason the drop fails, subsequent calls could return a new `submissionId`.
(This is unlikely under normal circumstances, but could result from racing concurrent requests.)

If the account address is well-formed, but the account does not exist in a finalized state on the chain, this call fails with a **404 Not found** status code.


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
`m₀`) amounts. They are amounts with indices `a_{i₀-m},..,a_{i₀}`

### Case where `selfAmounts` are the same

The only change then would have been the arrival of new incoming encrypted
amounts. In this case then `j₀ ≥ i₀` and `m₀ ≥ i₀`.

The amount `b_{j₀}` might not yet be seen, however we know that it is the
aggregation of amounts with indices `b_{j₀-m},..,a_{j₀}`, which we already
have (partially) decrypted, and then we can count those towards the public
balance already, while continuing to decrypt the remaining ones.

### Case when `selfAmount` are different

In this case the wallet must have made some action to cause this (or the user
has used account externally), in both of these cases you can do a similar
analysis with indices to determine the partial balance that you have already
decrypted, but of course in this case it will change.
