# Account preparation fixture provenance

Review status: source-derived expectations; independent human review pending.
No expected result was recorded from replacement-service output.

## HTTP contract

`src/Proxy.hs`, `getAccountNonceR` and `getAccountEncryptionKeyR`, defines
`GET /v0/accNonce/{address}` and `GET /v0/accEncryptionKey/{address}`.
Both parse the address before making a node call. Both override only the
NOT_FOUND message to `Account does not exist`, retaining HTTP 404 and error 2.
`doParseAccountAddress` supplies HTTP 400/error 1 for malformed addresses.
`src/Internationalization/En.hs` defines the exact English messages.
`runGRPCWithCustomError` defines the shared dependency-error mapping.

The nonce response follows `NextAccountNonce` in
`deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Types/Queries.hs`:
its derived fields are `nonce` and `allFinal`, with a numeric nonce. The adapter
fixtures deliberately exercise zero and u64::MAX and both reliability flags.
These boundary values are representation tests, not claims that a live node
would issue every value for an ordinary account.

The encryption-key handler reads account info at `Best`, not LastFinal, and
returns only `accountEncryptionKey`. The key is the base16 serialization of two
compressed curve points. The local SDK fixture uses two identity points:
`c0` plus 47 zero bytes each, making a 96-byte key. The expected hex is written
explicitly in `accounts_rpc.rs`, independently of the production encoder.
The full account protobuf fixture is shared with the balance tests.

## Controlled checks

`rust/tests/accounts.rs` checks HTTP shapes, numeric precision, pending reliability,
validation-before-query, account-specific missing errors, and deadline failures.
`rust/tests/accounts_rpc.rs` checks actual SDK request address bytes, the Best
selector, zero-nonce decoding, and key encoding through local gRPC transport.
These tests do not call a public node or submit transactions. Existing CI runs
them through the integration target.

The shared node-cookie and SDK error-provenance compatibility blockers remain
applicable; these tests do not resolve or waive them.
