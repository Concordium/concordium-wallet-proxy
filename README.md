# wallet-proxy

## Overview

The wallet proxy provides the following endpoints:

* `GET /accBalance/{account number}`: get the balance on an account
* `GET /accNonce/{account number}`: get the next nonce for an account
* `GET /simpleTransferCost`: get the cost of a simple transfer
* `GET /submissionStatus/{submissionId}`: get the status of a simple transfer or credential deployment
* `PUT /submitCredential`: deploy a credential/create an account
* `PUT /submitTransfer`: perform a simple transfer


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

## Account Balance

The balance on an account can be queried as in the following example:

```console
$ curl -XGET localhost:3000/accBalance/4WHFD3crVQekY5KTJ653LHhNLmTpbby1A7WWbN32W4FhYNeNu8
{"currentBalance":959505,"finalizedBalance":969670}
```

The result is a JSON object with two optional fields:
- `currentBalance` is the balance on the account in the current best block;
- `finalizedBalance` is the balance on the account in the most recently finalized block.

If neither field is present, then the account does not currently exist on the chain.
If only `currentBalance` is present, then the account has been created, but its creation has not yet been finalized.
Otherwise, both fields will appear.
(All amounts are integers in the smallest denomination of GTU.)

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

#### `status`
One of the following values:
* `"absent"`: the transaction is not known to the node
* `"received"`: the transaction has been received but not yet added to any block
* `"committed"`: the transaction is present in one or more blocks, but not a finalized block
* `"finalized"`: the transaction is present in a finalized block

If a submission is finalized it will always stay finalized. A received
transaction can transition into either committed, directly to finalized, or to
absent. A transaction that is committed is most likely going to transition to a
finalized one, although it is technically possible that it will become absent as well.

#### `outcome`
This field is present if the `status` field is `committed` or `finalized`.
The possible values are:
* `"newAccount"`: the transaction created a new account
* `"newCredential"`: the transaction deployed a credential to an existing account
* `"transferSuccess"`: the transaction was a successful simple transfer
* `"invalidTargetAccount"`: the transaction attempted to send funds to an invalid account
* `"nonExistentAmount"`: the funds were insufficient for the transfer
* `"insufficientEnergy"`: not enough energy was allocated to this transaction
* `"malformedTransaction"`: the transaction was malformed and could not be correctly deserialized
* `"ambiguous"`: the transaction has different outcomes in different blocks

The `ambiguous` outcome only applies to `committed` transactions.

#### `transactionHash`
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is the hash of the transaction.

#### `sender`
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is either the account address of the sender for a simple transfer, or `null` for a credential deployment.

#### `cost`
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is not `ambiguous`.
The value is a number representing the actual cost of the transaction to the sender.
(The value is an integer in the smallest denomination of GTU.)

#### `to`
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `transferSuccess`.
The value is the account address of the recipient of the simple transfer.

#### `amount`
This field is present if the `status` field is `committed` or `finalized`, and the `outcome` field is `transferSuccess`.
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

If the transaction succeeds, the `outcome` field will contain "transferSuccess".
In case of failure for different reasons the `outcome` field can contain one of the following:
- "invalidTargetAccount"
- "nonExistentAmount"
- "malformedTransaction"
- "insufficientEnergy"


