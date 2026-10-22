# 06: Retrieve account balances including protocol-level tokens

- **Type:** AFK
- **PRD coverage:** UC 1, 23; token-inclusive balance portions

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete the token-inclusive account balance version using SDK-backed account and token queries. Preserve CCD information and enrich protocol-level token holdings with the existing token and account module-state representation.

## Acceptance criteria

- [ ] Token holdings preserve identifiers, raw amounts, decimals, module references, supply, metadata references, and applicable allow-list/deny-list state.
- [ ] Empty holdings, absent module-state properties, missing accounts, and protocol-dependent behavior match current implementation semantics.
- [ ] Node-operation sequencing and partial query failures produce compatible responses rather than silently dropping information.
- [ ] Reviewed HTTP fixtures and controlled adapter tests cover multiple tokens, boundary amounts, and dependency failures.
- [ ] Tests run in CI and do not implicitly adopt SDK serialization as the HTTP contract.

## Implementation status

Implemented `/v2/accBalance/{address}` on the existing balance service. The CCD
view is shared with v1. Existing accounts include `accountTokens` (including an
empty array); missing finalized accounts retain `{}`. Account/pool queries stay
pinned to the consensus finalized hash. Tokenomics runs before sequential token
lookups, each using `LastFinal` rather than the pinned hash, and the conditional
reward-period query runs afterwards. Token ordering and duplicates are retained;
a lookup failure aborts the entire response without a partial token list. V0/v1
do not perform token queries.

`rust/src/balances/tokens.rs` selects explicit token/state fields and raw amount
strings from SDK types. `token_state.rs` uses the SDK's public CBOR decoder API
to project recognized legacy fields, rather than adopting SDK composite JSON
serialization. Absent account state omits `state`; SDK decoding or recognized-
field validation failures render the original hex. Metadata URLs are returned
as references and never fetched. The proxy-owned term reader and direct
Ciborium dependencies have been removed. No SDK code or RPC boundary was
bypassed or modified.

**Approved compatibility decision:** the user explicitly waived preservation of
`_additional`. Unknown module, account-state, and metadata extension properties
are skipped using the SDK and are not emitted. This includes P11 `locks` and
`available`: they are neither preserved as additional hex nor promoted into
new first-class JSON fields. Known fields, omission rules, strict outer keys,
duplicate-key handling, metadata checksums, and governance-account formatting
remain explicit compatibility projections.

Twelve new tests cover multiple/empty holdings, absent versus empty/invalid
state, allow/deny-list optionality, zero/max-u64 amounts and decimals boundaries,
metadata/checksums/governance accounts, ignored extensions, duplicate holdings,
query sequencing, and partial failures. Two use actual local SDK gRPC transport,
one through a real HTTP listener. Hand-written CBOR/JSON fixtures and source
provenance are in `rust/tests/fixtures/tokens/`. No public node or database is
used, and no transactions are submitted. Existing CI includes these tests.

Validation: 68 proxy tests pass serially with `TMPDIR=/tmp`; four existing
PostgreSQL tests remain ignored. Proxy Clippy with warnings denied and package
formatting pass. The SDK path dependency still emits its two existing lifetime
syntax warnings. Observed CI execution is not claimed.

Not yet accepted: fixture/projection review, Task 05 acceptance, and the existing
node-cookie and SDK error-provenance blockers remain outstanding. The SDK's
configurable default CBOR nesting budget of 128 still applies. Decoder errors,
including exceeding that budget, now use the ordinary original-hex fallback;
there is no private HTTP 500 depth policy. Haskell has no matching explicit
budget, so sufficiently deep state may render as hex rather than an object.
The `_additional` waiver does not approve this separate decoder-limit difference;
that remains an acceptance decision or requires deeper-term support.

## Blocked by

- [05: CCD account balances](05-ccd-account-balances.md)
