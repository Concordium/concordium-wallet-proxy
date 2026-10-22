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

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
