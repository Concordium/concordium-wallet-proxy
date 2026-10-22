# 08: Discover accounts by public key

- **Type:** AFK
- **PRD coverage:** UC 18, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Expose existing public-key account discovery against the existing PostgreSQL account-key bindings, including the optional restriction to simple accounts. Preserve legacy key and address encodings without migrating data.

## Acceptance criteria

- [ ] Results, filtering, ordering, and any duplicate handling match current behavior for representative credential and key bindings.
- [ ] Malformed keys, unmatched keys, and database failures preserve the existing HTTP contract.
- [ ] SQLx queries are compile-time checked with committed offline metadata and CI validation.
- [ ] Real PostgreSQL tests use synthetic legacy-compatible bindings; reviewed HTTP fixtures cover normal, filtered, empty, and invalid cases.
- [ ] The externally owned schema is not migrated or replaced.

## Implementation status

Implemented `GET /v0/keyAccounts/{key}` in `rust/src/discovery.rs`, backed by
`PostgresHistory` and wired into the production router. The URL accepts raw
Ed25519 hex; the checked SQL query uses the legacy scheme-prefixed BYTEA key.
Responses explicitly select the five legacy fields. Key validation precedes
`onlySimple` validation; the first repeated query value wins. Trailing decoded
key bytes, duplicate bindings, credential-only ordering, stored address decoding,
and Persistent's wrapping Word8 conversion are retained.

Five new tests cover source-derived HTTP contracts and real PostgreSQL behavior.
The test-only schema now includes the legacy binding table without adding new
production constraints or migrations. Provenance, exact parser-library checks,
and independent-review status are in `rust/tests/fixtures/discovery.md`.
The existing CI integration and query-metadata checks include this capability.

Validation: all 81 tests pass serially against disposable local PostgreSQL 18,
including all six normally ignored database tests. Offline Clippy with warnings
denied, package formatting, and diff whitespace checks pass. Both SQLx metadata
files were regenerated from the representative schema and independently
regenerated into a temporary directory with identical results, using online
`cargo check --all-targets` and `SQLX_OFFLINE_DIR`. This also refreshes the existing
health-query metadata with SQLx 0.9 column-origin information. The SQLx CLI was
not installed; CI's CLI validation and PostgreSQL 16 execution have not been
observed. The SDK still emits its two existing lifetime-syntax warnings.

Not yet accepted: independent fixture review and Task 01 acceptance remain
outstanding. No production database was accessed or changed. Acceptance
checkboxes remain open pending review.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
