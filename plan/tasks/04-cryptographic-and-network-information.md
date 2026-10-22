# 04: Expose cryptographic parameters and network information

- **Type:** AFK
- **PRD coverage:** UC 13, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Expose existing cryptographic parameters, genesis identity, epoch length, and consensus information through SDK-backed behavior and project-owned HTTP compatibility representations. Preserve startup caching of cryptographic parameters rather than adding new caching semantics.

## Acceptance criteria

- [ ] Every existing capability returns compatible fields, omissions, numeric representations, units, and timestamps.
- [ ] Startup-acquired cryptographic parameters retain their existing versioned representation and lifecycle.
- [ ] Controlled SDK adapter tests and independently reviewed HTTP fixtures cover success, relevant protocol variants, and node failures.
- [ ] SDK serialization is reused only where tests prove compatibility; unsupported variants are surfaced as blockers.
- [ ] Tests run in CI without public-node dependence or production writes.

## Implementation status

Implemented the four existing routes in `rust/src/network.rs` and connected them
in executable startup. `/v0/global` serves the version-0 startup-acquired
parameters without further node calls. The other three routes query live
consensus through a narrow SDK adapter with cloned clients, bounded requests,
and configured reconnection attempts. HTTP consensus fields are selected
explicitly; optional slot/BFT fields are omitted and last-finalization time can
be null. Durations remain milliseconds and integers retain full precision.

Seven new tests cover source-derived HTTP expectations, cached parameters,
protocols 1–10 through controlled SDK transport, errors, deadlines, timestamp
precision, and shared HTTP behavior. Fixture provenance is in
`rust/tests/fixtures/network.md`. No public node or production writes are used;
the existing CI target includes these tests automatically.

Validation: formatting, Clippy with warnings denied, and diff checks pass.
`TMPDIR=/tmp cargo test --locked --offline --all-targets -- --test-threads=1`
passes 40 tests, with four existing PostgreSQL tests ignored. The default TMPDIR
points to a nonexistent directory in this environment. A parallel run also hit
the existing unreachable-node test's released-port race; it passed serially.

Not yet accepted: independent fixture review and Task 01 dependency acceptance
remain outstanding. SDK consensus conversion failures lose error provenance
(e.g. missing fields become `INVALID_ARGUMENT`, producing HTTP 502 rather than
Haskell's HTTP 500). Request/response node-cookie forwarding is also still
unsupported. These are documented release blockers, not approved exceptions or
claims of full compatibility. Future unsupported protocol variants require SDK
support and compatibility fixtures before acceptance.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
