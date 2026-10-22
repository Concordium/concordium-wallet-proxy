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

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
