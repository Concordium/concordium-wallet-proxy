# 07: Retrieve account nonces and encryption keys

- **Type:** AFK
- **PRD coverage:** UC 2–3, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver existing nonce and account-encryption-key queries through the SDK. Preserve pending-transaction reliability information and the current distinction between malformed addresses, unknown accounts, and node failures.

## Acceptance criteria

- [ ] Nonce responses match finalized and pending-transaction scenarios, including reliability indicators and boundary values.
- [ ] Encryption keys retain their existing encoding and response shape.
- [ ] Malformed and unknown accounts preserve current status codes, classifications, and localized messages, including existing quirks.
- [ ] Independently reviewed fixtures and controlled node adapter tests verify success, validation precedence, and dependency failures.
- [ ] Tests run in CI without live public-node reliance.

## Implementation status

Implemented both v0 account-preparation routes in `rust/src/accounts.rs` and
wired them into the production router. The SDK adapter uses the shared timeout
and reconnection policy, obtains the nonce directly, and queries account info
at `Best` for the base16 encryption key. HTTP projection explicitly selects
`nonce`/`allFinal` and `accountEncryptionKey`. Invalid addresses do not reach
the node; NOT_FOUND preserves the account-specific message and error code.

Four new tests cover finalized/pending reliability, zero/max-u64 nonce values,
key representation, validation precedence, missing accounts, deadline failures,
and actual local SDK transport with address and Best-selector assertions.
Provenance and independent-review status are in
`rust/tests/fixtures/accounts.md`. Tests use no public node or production effects
and are included by the existing CI integration target.

Validation: 72 tests pass serially with `TMPDIR=/tmp`; four existing PostgreSQL
tests remain ignored. Offline Clippy with warnings denied and package formatting
pass. The SDK emits its two existing lifetime-syntax warnings. CI execution has
not been observed.

Not yet accepted: independent fixture review, Task 01 acceptance, and the shared
node-cookie and SDK error-provenance compatibility blockers remain outstanding.
The acceptance checkboxes are therefore not marked complete.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
