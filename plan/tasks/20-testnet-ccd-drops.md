# 20: Request testnet CCD with interoperable persisted drop state

- **Type:** AFK
- **PRD coverage:** UC 19, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver optional normal and scheduled testnet CCD drops, preserving the current persisted signed-transaction format, recipient uniqueness, nonce handling, expiry, retries, signing configuration, and error behavior. Manage only the existing proxy-owned recipient table.

## Acceptance criteria

- [ ] Normal and scheduled drops preserve amount, schedule rules, energy, signatures, expiry, submission identifiers, and conditional responses.
- [ ] Existing Haskell-produced persisted transactions can be decoded/reused; Rust-produced state remains interoperable for rollback, supported by independent binary fixtures rather than a running Haskell test service.
- [ ] Tests cover repeated/concurrent requests, unique-recipient conflicts, transaction rejection/absence/finality, expired transactions, nonce invalidation, insufficient funds, disabled configuration, and dependency failures.
- [ ] Time and randomness are controlled or checked through narrowly specified reviewed invariants; actual database writes and node submissions are asserted.
- [ ] Real PostgreSQL tests and checked SQLx queries preserve existing schema/data without conversion; CI has no production effects.
- [ ] No unapproved behavior correction is introduced, including apparent legacy quirks; operations document exclusive active faucet ownership during cutover.

## Blocked by

- [07: Nonces and encryption keys](07-nonces-and-encryption-keys.md)
- [13: Credential and account submission](13-credential-and-account-submission.md)
- [15: Submission progress](15-submission-progress.md)
