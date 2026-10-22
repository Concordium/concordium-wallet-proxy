# 05: Retrieve legacy and current CCD account balances

- **Type:** AFK
- **PRD coverage:** UC 1, 23; CCD balance portions

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver the existing CCD balance versions end to end, including current versus finalized account state, staking, delegation, cooldowns, release schedules, encrypted amounts, and available balance. Use SDK-backed queries and explicit compatibility conversions.

## Acceptance criteria

- [ ] Existing CCD balance versions preserve fields, omissions, numeric representations, and protocol-dependent staking information.
- [ ] Missing accounts and accounts created but not finalized preserve current version-specific behavior.
- [ ] Available balance, release schedules, cooldown phases, validator suspension, and encrypted balances match reviewed expectations.
- [ ] Independently reviewed fixtures cover each version and important boundaries; controlled node tests verify relevant block selection and failure handling.
- [ ] Tests run in CI; SDK gaps and behavior exceptions block completion pending explicit decision.

## Implementation status

Implemented the v0/v1 CCD balance routes in `rust/src/balances.rs`, with explicit
HTTP conversions in `rust/src/balances/view.rs` and SDK operations in
`rust/src/node/balances.rs`. Both services use the shared dependency family and
a directly cloned node adapter, without trait objects or an extra node `Arc`.
Finalized account, pool, tokenomics, and conditional reward-period queries use
the finalized hash returned by consensus. V0 best-account and pool lookups use
the best-block selector, as in Haskell. Pending changes share one finalized
chain-parameter query. No new caching is introduced.

The user-approved pool suspension fix is committed in SDK submodule revision
`26be8f2`. Cargo now uses that submodule instead of crates.io SDK 10.0.0. Its
P11-based checkout still declares package version 9.0.1; the gitlink, not a
registry version, selects the supported source. CI initializes submodules
recursively and uses the repository's Rust 1.99 toolchain. SDK code was not
modified further in this slice. Existing consensus adapter tests also cover P11.

Sixteen new tests cover source-derived v0/v1 HTTP representations, absent
accounts/pools, encrypted aggregation, release order, all cooldown phases,
node-supplied and fallback available balances, zero/max-u64 boundaries,
validator/delegator suspension, payday rounding, conditional queries, and failures.
Four tests use actual local SDK gRPC transport, including concurrent HTTP
requests through a real local listener. No public node or database is required,
and no transactions are submitted. Fixture provenance and review status are in
`rust/tests/fixtures/balances/README.md`.

Validation: 56 proxy tests pass serially with `TMPDIR=/tmp`, four existing
PostgreSQL tests remain ignored, and proxy Clippy passes with warnings denied.
The SDK emits two existing lifetime-syntax warnings as a path dependency.
Formatting is checked for the proxy package rather than rewriting the SDK and
its nested path dependencies. Live PostgreSQL metadata validation and observed
CI execution are not claimed.

Not yet accepted: independent fixture review, Task 01 dependency acceptance,
node-cookie forwarding, and SDK malformed-response/error-provenance compatibility
remain outstanding. Unknown staking, cooldown, and pool-opening variants fail
rather than silently dropping data. The SDK prerequisite is resolved for the
implemented suspension fields, not a waiver of other compatibility blockers.
V2 token enrichment is intentionally left to Task 06.

Source observations to retain during implementation:

- This slice implements v0 and v1; token-inclusive v2 belongs to Task 06.
- v0 queries finalized state first, then best state; v1 only queries finalized.
- A missing finalized account returns `{}` without querying best state, even
  when the account could exist in an unfinalized block. The handler comment is
  not the implementation oracle for this quirk.
- Release, encrypted-amount aggregation, cooldown, and staking representations
  need explicit conversions rather than whole-account SDK serialization.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
