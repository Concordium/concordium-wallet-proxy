# 10: Inspect blocks and block transaction outcomes

- **Type:** AFK
- **PRD coverage:** UC 22, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Expose existing block information, blocks-at-height, and block transaction-outcome queries using the SDK and project-owned compatibility conversion. Resolve event support required by these capabilities within this slice.

## Acceptance criteria

- [ ] Block selection, heights, hashes, timestamps, outcomes, ordering, and conditional fields match current behavior.
- [ ] Fixtures exercise supported historical and current event variants, including sponsorship and lock-related events where applicable.
- [ ] Invalid references, missing blocks, empty results, and node failures preserve existing responses.
- [ ] Independently reviewed HTTP fixtures and controlled SDK adapter tests run in CI.
- [ ] Unsupported SDK variants block completion; no silent omission, fork, or protobuf bypass is introduced.

## Implementation status

Implemented all three v0 block routes through a narrow `BlockNode` interface and
SDK-backed adapter. Block information explicitly preserves legacy fields, nulls,
protocol validation, and UTC timestamp formatting. Height queries are absolute
and preserve node ordering and duplicates. Hash parsing retains uppercase and
trailing-whitespace acceptance, while malformed hashes and heights keep their
distinct legacy errors. The shared timeout covers complete outcome-stream
consumption; late stream failures do not return partial HTTP success.

`rust/src/blocks/outcomes/` owns the compatibility projection for all supported
SDK account-effect categories, contract traces, validator/delegation events,
credential and token creation, governance updates, sponsorship, and rejection
records. It does not use SDK BlockItemSummary serialization as the HTTP contract.
Selected SDK leaf serializers supply cryptographic representations; field lists,
event ordering, actor roles, costs, optional fields, payload names, key-level
update types, and legacy quirks remain explicit. Scheduled releases use numeric
milliseconds. Mint-rate conversion is shared with staking. Governance projection
also corrects SDK/Haskell differences in root-key update classifications, GAS
rewards CPV2 summary types, optional authorization fields, `createPLT` spelling,
and the legacy standalone committee min/max serialization swap.

Twenty new tests cover block metadata, boundaries, all account-effect categories,
contract and staking event variants, sponsorship, tokens, representative update
payloads and key versions, rejections, unknown variants, and stream behavior.
Three tests use local SDK/gRPC transport; one also uses a real HTTP listener.
Source-derived expectations, field provenance, and remaining limits are in
`rust/tests/fixtures/blocks.json` and `blocks.md`. No production effects, public
node, new database query, schema change, or additional SDK edit is involved.
Existing CI includes these tests. Futures-util is now a direct dependency for
SDK stream collection, using the already locked version.

Validation: all 114 tests pass serially, including all six normally ignored
PostgreSQL tests, against disposable local PostgreSQL 18. Locked offline Clippy
with warnings denied, package formatting, and diff whitespace checks pass. The
SDK emits its two existing lifetime-syntax warnings. CI execution and controlled
real-node release acceptance have not been observed.

Not yet accepted: independent fixture review, Task 01 acceptance, and the shared
node-cookie, SDK error-provenance, and timestamp-range limits remain outstanding.
The legacy API excludes lock-only events and rejects unsupported P11 block
metadata, lock rejection reasons, and maximum-lock-duration updates. Token-event
projection relies on the node's documented duplicate legacy/unified token lists;
if malformed lists disagree, the SDK has discarded the original and exact legacy
reconstruction is unavailable. No exception or raw protobuf bypass is approved.
Acceptance checkboxes therefore remain open.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
