# 17: Browse protocol-level token and lock-related history

- **Type:** AFK
- **PRD coverage:** UC 10–11, 23; token-inclusive history

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete the token-inclusive history version, retaining its CCD behavior and adding existing protocol-level token creation, governance, holder, and lock-related summary/event representations. Resolve legacy summary and SDK support within this slice.

## Acceptance criteria

- [ ] Token and CCD selection, version boundaries, filters, pagination, and response details match current behavior.
- [ ] Supported governance operations, sponsored effects, lock creation/destruction, and token transfers with lock metadata decode and render compatibly where currently exposed.
- [ ] Real PostgreSQL fixtures cover historical/current summary forms, token rejections, mixed activity, and applicable optional event metadata.
- [ ] Reviewed HTTP fixtures and checked SQLx metadata are validated in CI alongside legacy-history regression tests.
- [ ] Unsupported formats or dependency variants remain explicit blockers; no event or transaction is silently discarded.

## Implementation status

Implemented `GET /v3/accTransactions/{address}` on the existing history service.
Checked static queries include token/meta/creation rows only in v3 and retain
CCD, suspension, reward, cursor, limit, and time/height behavior. The legacy
decoder now renders token creation, governance/module events, mint/burn, account
holders, singleton transfers with optional lock/memo metadata, and lock
creation/destruction. Sponsors retain separate CCD fee attribution; token
amounts do not become CCD subtotals. Unknown selected formats fail the whole page.

Resolved the missing persisted meta/lock schemas using the original lock-support
commits and the three committed Haskell fixtures. The current SDK supplies the
field types; persisted meta names and historical return rejections have explicit
legacy decoders. No SDK fork, protobuf bypass, schema migration, Haskell source
edit, or dependency-pin change is introduced. Current SDK release/duration
rejections are not guessed to be historical return variants. Details and exact
source revisions are in `rust/tests/fixtures/token-history.md`.

Twelve new tests cover source fixtures, optional/conditional details, governance
text, raw rejection omission/null distinctions, numerical/byte boundaries,
unsupported formats, and a real PostgreSQL + local SDK/gRPC + HTTP mixed-history
slice. Existing CCD rendering/request tests and the database filter matrix now
include v3. Legacy Word64 token-display wraparound and its divide-by-zero failure
boundary are preserved rather than silently replaced with different behavior.

Validation: 234 tests pass serially (four unit and 230 integration), including all
twelve disposable PostgreSQL tests. Regenerated SQLx metadata matches all four
live descriptions. Locked offline compilation, package formatting, Clippy with
warnings denied, and whitespace checks pass. Existing SDK lifetime-syntax
warnings remain unchanged.

Not yet accepted: independent schema/fixture/wording/format review, Task 16 and
Task 01 acceptance, observed CI, baseline/deployed dependency alignment, and
controlled real-node release evidence. The checked-out Haskell pin mismatch and
Task 15's node-wire limitations are not claimed fixed by a stored-JSON decoder.
Any additional supported persisted format without established authority blocks
acceptance; it is not silently omitted or deferred to Task 22. Acceptance
checkboxes remain open pending that evidence.

## Blocked by

- [16: CCD transaction history](16-ccd-transaction-history.md)
