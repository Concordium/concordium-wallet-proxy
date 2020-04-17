A GET request to `/submissionStatus/[transactionHash]` returns a JSON object summarizing the status of a simple transfer or credential deployment.

## Successful outcomes
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


## Failure outcomes

The following failure modes are possible:
* 502 (Bad gateway): the proxy failed to communicate with the GRPC endpoint of the node.
* 400: The transaction hash was invalid, the transaction was of the wrong type, or the response from the GRPC endpoint could not be decoded.

In such a case, the response will be a JSON object of the following form:
```
{"error":1,"errorMessage":"Malformed transaction hash."}
```