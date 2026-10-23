# 09: Expose staking pools, delegation, and chain conditions

- **Type:** AFK
- **PRD coverage:** UC 14, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver validator-pool status, passive delegation status, chain parameters, and next-payday queries through SDK-backed operations with explicit wallet-facing representations.

## Acceptance criteria

- [ ] Existing protocol-dependent values, optional properties, time calculations, and numeric representations remain compatible.
- [ ] Unknown pools, invalid path values, unsupported protocol conditions, and node errors match current behavior.
- [ ] Reviewed request/response fixtures cover each capability and important protocol transitions and boundaries.
- [ ] Controlled adapter tests verify node query selection and error mapping; time-dependent tests use controlled time where necessary.
- [ ] Tests run in CI; dependency gaps require an explicit decision rather than omitted fields or endpoints.

## Implementation status

Implemented all four existing v0 routes: baker pool, passive delegation, chain
parameters, and next payday. `rust/src/staking/` explicitly projects pool fields
and chain-parameter versions 0–3 rather than exposing SDK composite JSON.
Optional suspension and missed-round fields retain omission semantics; absent
current-payday status remains null. Amounts retain micro-CCD strings, parameter
names and units remain legacy-compatible, and mint decimals support the full
mantissa/exponent range without the SDK Serde decimal limit.

The SDK adapter retains the pool response hash, resolves the foundation account
index with the original chain-parameter selector, and preserves pending-change
query order: pinned parameters/account, pinned tokenomics, then live consensus.
Inactive pools, malformed pool paths, missing pools, old-protocol payday queries,
and later dependency failures have distinct legacy error handling. Exact fraction
and payday helpers are shared with balances; payday period multiplication now
also preserves Haskell's Word64 wrapping behavior.

The user-authorized SDK support for `missed_rounds` is available at revision
`f51125d`; no further SDK edits were needed. Seven controlled-adapter HTTP tests,
three local SDK/gRPC tests (one with an HTTP listener), and three unit tests cover
version transitions, numeric and time boundaries, omissions, query selection,
foundation lookup, and failures. Source-derived fixtures, provenance, and limits
are in `rust/tests/fixtures/staking.json` and `staking.md`. Existing CI includes
the tests without public-node reliance or production effects.

Validation: all 94 tests pass serially, including the six normally ignored
PostgreSQL tests, against a disposable local PostgreSQL 18 server. Offline Clippy
with warnings denied, package formatting, and diff whitespace checks pass. The
SDK still emits its two existing lifetime-syntax warnings. Observed CI execution
is not claimed. No database schema or query changes were needed.

Not yet accepted: independent fixture review, Task 01 acceptance, and the shared
node-cookie and SDK error-provenance blockers remain outstanding. Computed
paydays also retain the inherited Chrono date-range limit documented in fixture
provenance; it is not a compatibility waiver. Acceptance checkboxes remain open.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
