# 14: Submit sponsored and raw transactions

- **Type:** AFK
- **PRD coverage:** UC 7–8, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete existing extended/sponsored transaction submission and bare binary transaction submission. Preserve protocol-level sponsorship, sender/sponsor authorizations, transaction contents, and existing binary acceptance rules.

## Acceptance criteria

- [ ] Valid sponsored submissions preserve sender and sponsor signatures and return compatible identifiers.
- [ ] Raw binary fixtures cover supported transaction kinds and representative malformed, incomplete, and unsupported encodings.
- [ ] Validation order, node rejection, dependency errors, and HTTP metadata match current behavior.
- [ ] Controlled adapter assertions verify submitted bytes or equivalent complete transaction content and prevent production effects.
- [ ] Reviewed fixtures and serialization tests run in CI; SDK gaps block completion without a silent fork or bypass.

## Implementation status

Implemented the existing `submitExtended` JSON and `submitRawTransaction` binary
PUT routes through the shared SDK submission path. Extended requests preserve
sender/sponsor maps, bitmap/header contents, raw payloads, optional sponsor
semantics, and locally calculated identifiers. Sponsor consistency remains a
node check; no alternate sponsorship model or pre-submission repair is added.

Raw input supports all four legacy item tags, including both credential forms
and account transactions V1. The SDK exposes the typed V1 decoder and submission
variant even though its generic block-item decoder omits tag 3; the local
adapter composes those supported APIs. Governance updates enforce the legacy
hardcoded P10 payload and authorization gates, consume the isolated payload,
and serialize its typed representation. Account payloads remain opaque. Outer
trailing bytes are ignored and do not affect submitted contents or identifiers.
No SDK code, direct protobuf submission path, signing, or new retry policy is
introduced.

The user's error-equivalence rule is carried forward for failing requests: both
implementations must return an error, but its text/classification can differ,
including SDK-local rejection instead of node rejection for malformed proofs.
Successful contents/identifiers and retry behavior remain compatibility
requirements; success-versus-error differences are not waived.

Seventeen new integration tests cover source-defined bytes and golden hashes,
all raw kinds, sponsorship/authorization details, P10 versus SDK acceptance,
malformed and noncanonical inputs, exact SDK/gRPC operations, transport replay
limits, timeout, and real HTTP metadata/CORS/method/redirect behavior. Provenance
is in `rust/tests/fixtures/submission-extended.md`. Tests use local adapters and
mock endpoints only; there are no production effects or database changes.

Validation: 167 tests pass serially (four unit and 163 integration); six existing
PostgreSQL tests remain ignored. Locked offline Clippy with warnings denied,
package formatting, and whitespace checks pass. Tests use `TMPDIR=/tmp`; the SDK
retains its two existing lifetime-syntax warnings.

Not yet accepted: independent fixture/binary review (including remaining P10
governance layouts), Task 13/Task 01 acceptance, observed CI, and controlled
real-node release acceptance. Shared node-cookie, malformed-node-hash provenance,
and JSON/server resource-limit review remain open. Acceptance checkboxes are
therefore not marked complete.

## Blocked by

- [13: Credential and account submission](13-credential-and-account-submission.md)
