# Wallet Proxy API

This document describes the API exposed by the Wallet Proxy

## Overview

The wallet proxy provides the following endpoints:

* `GET /v0/health`: get a response specifying if the wallet proxy is up to date

WIP: Add endpoints as implemented including a section below that describes the endpoint 

## Errors

In case a request cannot be processed, the service will respond with the proper HTTP status code. 
In addition, the response body will be a JSON object with the error message and a (custom) error code
```json
{
  "errorMessage": "free form string",
  "error": <error code>
}
```

The error codes currently returned are
- 0: internal server error, most likely the server could not communicate with the node:
  ```json
  {"error":0,"errorMessage":"Error accessing the GRPC endpoint"}
  ```
- 1: the request is invalid. There can be a number of reasons for this, but most of them
  should not occur once the initial debugging is over. They indicate that data is malformed in one
  way or another. The message will usually pinpoint the cause of the error.
  ```json
  {"error":1,"errorMessage":"Malformed transaction hash."}
  ```
- 2: the request is well-formed, but the requested object could not be
  found, e.g., the account does not exist in the expected place.

### Language & localization

Error messages and textual descriptions will be localized by the proxy based on the `Accept-Language` header.
(Currently, only `en` is available.)
Languages can also be specified with the `_LANG` GET-parameter and/or a `_LANG` cookie; these have higher precedence than `Accept-Language`.

TODO: update this section as part of COR-1814
