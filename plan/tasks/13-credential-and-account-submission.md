# 13: Submit credential deployments and account transactions

- **Type:** AFK
- **PRD coverage:** UC 5–6, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver existing credential-deployment and standard account-transaction submission paths from accepted request formats through SDK node submission to compatible submission identifiers and errors.

## Acceptance criteria

- [ ] Supported credential versions and existing standard transaction request formats decode compatibly and preserve submitted content.
- [ ] Accepted submissions return the expected identifier and response; malformed requests, node rejection, and transport failures match current behavior.
- [ ] Reviewed request/response fixtures and deterministic serialization tests verify identifiers and input interpretation.
- [ ] Controlled adapters assert exact submitted operations and retry behavior, not only HTTP responses.
- [ ] Tests never submit into production and run in CI; no unapproved retry policy or unsupported-format omission is introduced.

## Implementation status

Implemented the credential and standard account-transaction PUT routes through
SDK-backed submission. Both credential types use the SDK's structured types;
account transaction payloads remain opaque. The proxy computes identifiers from
the exact submitted item rather than trusting the node's response hash. It does
not add signing, expiry checks, payload interpretation, or a new retry policy.

Fourteen new integration tests verify complete source-derived bytes and golden
identifiers, malformed inputs under the approved decision, node rejection,
duplicate/trailing JSON and binary handling, exact local gRPC operations, timeout,
and the existing one-reconnect limit. The normal credential identifier also
matches the legacy README. A real HTTP test covers standard submission without
a JSON content-type requirement. No production node, SDK edit, or database
change is involved. Provenance is in `rust/tests/fixtures/submission.md`.

Validation: 150 tests pass serially (four unit and 146 integration); six existing
PostgreSQL tests remain ignored. Locked offline Clippy with warnings denied,
package formatting, and whitespace checks pass. The environment requires
`TMPDIR=/tmp`; the SDK retains two existing lifetime-syntax warnings.

Acceptance remains open for independent fixture and lexer review, Task 01,
observed CI, and controlled real-node release validation. Node cookie forwarding
and the SDK's response-hash error provenance remain shared blockers: a malformed
hash in an otherwise successful node response can cause Rust rejection where
Haskell returns the locally calculated identifier. This success-versus-error
difference is not waived by the approved error-equivalence decision. JSON depth
and server resource-limit compatibility also require review.

The Haskell `CredentialDeploymentInformation` parser treats `proofs` as opaque
Base16 bytes and forwards structurally invalid proofs to the node. See
`deps/concordium-client/deps/concordium-base/haskell-src/Concordium/ID/Types.hs`,
`FromJSON CredentialDeploymentInformation`. Node rejection becomes HTTP 400
with `Credential rejected by the node` through `doSendBlockItem` in `src/Proxy.hs`.

The pinned SDK's `BlockItem::CredentialDeployment` requires a typed
`AccountCredentialMessage`, whose normal credential contains structured
`CredDeploymentProofs`. Its deserializer parses every proof component, so it
cannot represent arbitrary legacy proof bytes, including an empty proof. The
public `v2::Client::send_block_item` API accepts only this typed block item;
there is no opaque credential-deployment input. Relevant SDK sources are
`concordium-base/rust-src/concordium_base/src/id/types.rs` (credential/proof
structures) and `src/v2/mod.rs` (submission method and request conversion), under
`concordium-rust-sdk/`.

## Approved error-equivalence decision

The user approved different errors when a request results in an error in both
implementations. For Task 13 this permits SDK-local rejection of malformed
credential proofs instead of forwarding them for node rejection, and does not
require the same error text or classification. A rejected request must not be
reported as successful. This removes the opaque malformed-proof blocker; it
does not permit rejecting requests that would succeed in Haskell, changing
successful submission contents or identifiers, or changing retry policy. It does
not approve a private SDK fork or protobuf bypass. Other tasks' error contracts
are not changed by this scoped decision.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
