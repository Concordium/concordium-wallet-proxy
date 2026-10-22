# 02: Serve versioned identity-provider catalogues

- **Type:** AFK
- **PRD coverage:** UC 12, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Serve every existing identity-provider catalogue version from validated startup configuration. Preserve public provider information, issuance and recovery metadata, company-provider inclusion rules, and historical wallet flows.

## Acceptance criteria

- [x] Existing catalogue versions return compatible content, conditional fields, and HTTP behavior without requiring wallet changes.
- [x] Startup rejects invalid required catalogue data before serving.
- [ ] Independently reviewed request/response fixtures cover each version, representative providers, and applicable empty or malformed configuration scenarios.
- [ ] Handler and configuration tests run in CI without production dependencies; shared errors and language behavior remain compatible.
- [x] Any implementation/documentation discrepancy follows current Haskell behavior; unresolved exceptions are surfaced, not silently changed.

## Implementation status

Implemented and locally verified with six socket-free catalogue tests. Source-derived
HTTP fixtures cover all three versions, duplicate verification backends, company
identity flows, null/optional/extension values, large numbers, errors, redirects,
HEAD, and public CORS. Additional tests cover concurrent requests, startup snapshots,
and malformed or missing files for every version. Catalogue requests cannot call
the node, database, or clock in these tests.

The existing Haskell handlers return configured JSON values unchanged. Catalogue
files, not handlers, control provider inclusion. The implementation accepts any
valid JSON value, despite the README's array/schema description. This compatibility
choice is documented and tested; no new runtime filtering or schema rules are added.

Independent human fixture review and an observed CI run remain pending, so final
acceptance is not marked complete. CI includes the new tests automatically. The
real PostgreSQL/SDK/HTTP startup test now also checks catalogue responses and their
availability after database failure; local database/socket restrictions still
prevent running that test here. Task 01's SDK compatibility blockers remain open.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
