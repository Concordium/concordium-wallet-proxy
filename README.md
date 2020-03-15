# wallet-proxy

## Credential deployment/account creation

Using the example credential in [examples/cdi.json](examples/cdi.json) the
following displays an interaction that leads to credential deployment.

```console
$ curl -XPUT -d "@examples/cdi.json" localhost:3000/submitCredential
{
  "submissionid": "f1a3a3c17e3bc70a4dad3f409265f8a1fca07c607b732e11cf279dd2e891e0af"
}
```

If the credential is well-formed and communication with the server is good, and
the baker node is alive the server will respond with a submission id which can
be queried later on. Otherwise 

- if JSON request body is not a valid credential then the server responds with
  error code 400 and a JSON object with a single field `error` in the body.
- if there is a communication problem with the baker the response code is 502
  (Bad Gateway) and body is Null.
- if there are any other problems with the request (e.g., duplicate submission)
  we reply with error code 400 and a JSON object with a single field `error`

If the submission is successful then the returned `submissionid` can be used to
query the status of the submission as in the following example.

```console
$ curl -XGET localhost:3000/submissionStatus/f1a3a3c17e3bc70a4dad3f409265f8a1fca07c607b732e11cf279dd2e891e0af
{
  "status": "finalized",
  "95637bb7647d9a2b5c2cac64916ec4165685862c349a38237a35349a180d1a7f": {
    "hash": "f1a3a3c17e3bc70a4dad3f409265f8a1fca07c607b732e11cf279dd2e891e0af",
    "sender": null,
    "cost": 0,
    "result": {
      "events": [
        {
          "tag": "AccountCreated",
          "contents": "4a3wqcAdVz7QC34rxyXNzRwYMgBKPBNYWfyTBnjAaLpeR6H3pR"
        },
        {
          "tag": "CredentialDeployed",
          "regid": "b341a8e766932bd728f186a669d49f7a91e016f59a2bbfa206a9eca0a242a818626eda2f7f58e10ed13ac9d5fae7f2ff",
          "account": "4a3wqcAdVz7QC34rxyXNzRwYMgBKPBNYWfyTBnjAaLpeR6H3pR"
        }
      ],
      "outcome": "success"
    },
    "energycost": 9990,
    "type": null,
    "index": 0
  }
}
```

In this particular case we can see the transaction has already been finalized.

In general the return values possible from the call are
- status code 502 and Null as the body
- status code 400 if the submission id is ill-formed (not a valid SHA256 hash)
  and an object with a single field `error` describing the error.
- status code 200 and Null as the body (this means there is no submission with
  that ID known to the baker)
- ```json
  {
    "status": "received"
  }```
  When the submission has been received, but it is not yet committed to any
  blocks and no outcomes are known.
  
- ```json
  {
    "status": "committed",
    "$BLOCKHASH": "$OUTCOME"
  }```

where there can be one or more pairs of block hash and outcome, and outcome
always has the format as above in the case of a finalized block. For
credential submission the format is always going to be the same as above,
except that `events` can be different. It is always a list of objects. For
credential deployment the possible results are

  - ```json
     [
        {
          "tag": "AccountCreated",
          "contents": "4a3wqcAdVz7QC34rxyXNzRwYMgBKPBNYWfyTBnjAaLpeR6H3pR"
        },
        {
          "tag": "CredentialDeployed",
          "regid": "b341a8e766932bd728f186a669d49f7a91e016f59a2bbfa206a9eca0a242a818626eda2f7f58e10ed13ac9d5fae7f2ff",
          "account": "4a3wqcAdVz7QC34rxyXNzRwYMgBKPBNYWfyTBnjAaLpeR6H3pR"
        }
      ]

  - ```json
     [
        {
          "tag": "CredentialDeployed",
          "regid": "b341a8e766932bd728f186a669d49f7a91e016f59a2bbfa206a9eca0a242a818626eda2f7f58e10ed13ac9d5fae7f2ff",
          "account": "4a3wqcAdVz7QC34rxyXNzRwYMgBKPBNYWfyTBnjAaLpeR6H3pR"
        }
      ]
    
  The first will happen in the case a credential is creating an account, and the
  second when a new credential is being deployed onto an existing account.
