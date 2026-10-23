# 11: Estimate fees for transfers, staking, and delegation

- **Type:** AFK
- **PRD coverage:** UC 4, 23; non-contract and non-token estimates

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver all current transfer, encrypted-transfer, staking, and delegation fee-estimation modes, including configure-validator options and existing aliases. Use SDK/base primitives where compatible and preserve current wallet-facing calculations.

## Acceptance criteria

- [ ] Energy and CCD costs preserve signature counts, memo sizes, metadata sizes, commission options, suspension options, defaults, and rounding.
- [ ] Existing transaction aliases yield compatible results and parameter-presence semantics are preserved.
- [ ] Missing or malformed parameters, numeric boundaries, protocol restrictions, and node failures match validation order and errors.
- [ ] Reviewed fixtures and calculation/adapter tests cover all supported modes and relevant combinations.
- [ ] CI runs deterministic tests without a public node; unapproved calculation changes are not introduced.

## Implementation status

Implemented the v0 transaction-cost route for all 13 transfer, encrypted-transfer,
validator, and delegation aliases. Query handling preserves first values,
presence-only flags, defaults, ignored fields, and the aliases that ignore
sponsorship. Memo parsing precedes the protocol-1 rejection. Payload and energy
arithmetic retain legacy wrap points, while exact rational cost conversion
preserves upward rounding and Word64 Amount boundaries.

The narrow `FeeNode` SDK adapter reads consensus followed by chain parameters at
Best, before input validation, without resolving the foundation account. SDK
operation constants are reused; its u32 signature-count base calculation and
modulo-u64::MAX price helper are not used where they differ from Haskell.

Ten new tests include source-derived HTTP fixtures, all 512 configure-validator
field combinations, numeric and pricing boundaries, sponsorship rules, and real
local SDK/gRPC transport for four chain-parameter versions. Provenance and limits
are in `rust/tests/fixtures/fees.md`. No Haskell files, SDK code, database queries,
or production services changed. Contract and token estimates remain Task 12.

Validation: 118 tests pass serially (three unit and 115 integration); six existing
PostgreSQL tests are ignored and were not run for this task. The environment's
missing default temporary directory required `TMPDIR=/tmp`. Locked offline Clippy
with warnings denied, package formatting, and whitespace checks pass. The SDK
still emits its two existing lifetime-syntax warnings.

Not yet accepted: independent fixture and full integer-lexer review, observed CI,
controlled real-node release acceptance, and Task 01 acceptance. The shared
node-cookie and SDK error-provenance limits remain release blockers; malformed
chain parameters can surface as HTTP 502 INVALID_ARGUMENT rather than Haskell's
local-decoding failure. Acceptance checkboxes remain open; no exception is
approved.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
