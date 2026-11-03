# 15: Report submission progress and transaction outcomes

- **Type:** AFK
- **PRD coverage:** UC 9, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver submission-status lookup for transaction hashes and existing submission identifiers, with compatibility conversion for conditional outcome information across the transaction lifecycle.

## Acceptance criteria

- [ ] Absent, received, committed, and finalized states retain existing response behavior.
- [ ] Success, rejection, and ambiguous committed outcomes preserve block references, descriptions, and conditional transaction details.
- [ ] Covered details include transfer/encrypted effects, credential deployment, contract initialization/update traces, and other currently represented variants.
- [ ] Malformed identifiers, inconsistent node responses, and transport failures match existing error behavior.
- [ ] Reviewed fixtures and controlled node adapter tests run in CI; omitted-versus-null distinctions and numeric formats are asserted.

## Implementation status

Implemented the v0 submission-status GET route through a narrow read-only SDK
adapter. Hash validation precedes lookup. Node NOT_FOUND yields absent; received,
committed, and finalized responses retain their distinct fields and successful
Content-Language header. Block references are sorted, duplicate block entries
retain the last decoded outcome, and missing/inconsistent results fail without
partial output.

Committed comparisons retain the complete legacy common and detail fields,
including the original sender before an encrypted event overrides it. Index and
energy differences are ignored as in Haskell. Differing supported outcomes return
only status, blockHashes, and ambiguous; later unsupported outcomes are not
hidden by earlier ambiguity.

Supported details include transfers/memos, encrypted effects, credential creation,
validator/delegation configuration, modules, contract initialization, the four
legacy contract trace kinds, token transfers, registered data, sponsorship,
and English reject descriptions. Contract upgrades and other unsupported
successful types keep the legacy failure behavior. Non-single-transfer token
successes retain the no-outcome-property quirk. The existing block event
projection is shared rather than exposing SDK summary JSON. Haskell Show quoting
was moved unchanged from fee handling into shared compatibility code.

Twenty-five new integration tests cover lifecycle states, conditional fields,
precision and omission/null distinctions, ambiguity, rejection wording and
quirks, SDK/gRPC input and response conversion, malformed responses, timeouts,
and real HTTP language/metadata/HEAD/method/redirect behavior. Source provenance
and remaining limits are in `rust/tests/fixtures/progress.md`. No submission,
database query, SDK edit, or protobuf bypass is introduced.

Validation: 192 tests pass serially (four unit and 188 integration); six existing
PostgreSQL tests remain ignored. Locked offline Clippy with warnings denied,
package formatting, and whitespace checks pass. Tests use `TMPDIR=/tmp`; the SDK
retains its two existing lifetime-syntax warnings.

Not yet accepted: independent fixture/rejection-format review, Task 01 acceptance,
observed CI, and controlled real-node release acceptance. Meta/lock branches in
Proxy.hs are not reachable through the pinned Haskell adapter's legacy token
conversion; newer deployed dependency/wire behavior must be established before
claiming those successful status fields. Shared token-list reconstruction,
node-cookie, and SDK error-provenance limits remain documented.

The user's error-equivalence rule is retained for cases where both implementations
fail. It does not waive successful HTTP 200 reject descriptions, success fields,
absence versus query failure, or ambiguity semantics. Acceptance checkboxes remain
open; newer successful fields are not guessed or silently claimed compatible.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
