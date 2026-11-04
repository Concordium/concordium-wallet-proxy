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

## Implementation status

Implemented the optional `/v0/testnetGTUDrop/{address}` route in
`rust/src/faucet.rs`, with separate transaction signing, recipient storage, and
SDK adapter modules. Startup loads the legacy address/accountKeys JSON before
dependency connections; `--drop-account` / `WALLET_PROXY_DROP_ACCOUNT` enables
the faucet, and `--drop-amount` / `WALLET_PROXY_DROP_AMOUNT` uses decimal CCD
with the existing 2000 CCD default. Only the proxy-owned `g_t_u_recipient` table
is created when enabled; externally owned history tables are not migrated.

New transactions are stored as bare signature/header/payload bytes before
submission. Existing rows preserve received/committed/successful-finalized reuse,
absent/rejected resubmission, strict expiry and nonce replacement, recipient
uniqueness, and conflict retries. Intentional legacy quirks remain: retry checks
use the recipient's balance and nonce, the first scheduled release is ten minutes
from rounded current time, and submission failures return HTTP 400 configuration
errors. JSON prefix/first-duplicate parsing is now shared with submission code
without changing submission behavior.

Evidence is in `rust/tests/fixtures/faucet.md`: normal and scheduled binary
fixtures are authored with an independent Python serializer/Ed25519 signer, not
the replacement SDK. Tests check exact new bytes against those vectors and reuse
them as persisted state. These fixtures are not captures of Haskell-produced rows;
actual historical-state/rollback evidence remains outstanding. Three real
PostgreSQL tests cover committed byte visibility before submission, a forced
concurrent HTTP insert conflict, unique recipients, row-specific deletion,
existing-state reuse, and preservation of unrelated tables. The production
status adapter has controlled SDK transport coverage for pending, successful,
rejected, absent, malformed, and failed outcomes. No public node or funding
account is used.

Validation: all 279 integration tests (including all 17 normally ignored database
tests) and 5 unit tests passed against a disposable PostgreSQL 18 cluster.
All-target live SQLx checking regenerated metadata identical to the committed
cache, including the three new recipient queries. All-target Clippy with
`-D warnings` passed; the SDK retains its two existing lifetime-syntax warnings.
Operator instructions in `rust/README.md` require exclusive active faucet
ownership, draining before cutover/rollback, and no state-changing shadow traffic.

Not yet accepted: independent schema/fixture/signature/parser review, actual
Haskell-produced-state and rollback evidence, observed CI, Task 07/13/15
acceptance, shared deployed dependency alignment, and controlled real-node
end-to-end acceptance remain open. No SDK pin or external schema was changed;
no production faucet activation or traffic switch is authorized.

## Blocked by

- [07: Nonces and encryption keys](07-nonces-and-encryption-keys.md)
- [13: Credential and account submission](13-credential-and-account-submission.md)
- [15: Submission progress](15-submission-progress.md)
