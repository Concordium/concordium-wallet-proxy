# 18: Discover contract tokens and retrieve metadata and balances

- **Type:** AFK
- **PRD coverage:** UC 15–16, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver existing CIS-2 token discovery from PostgreSQL plus all existing metadata-reference and token-balance query versions through SDK-backed contract invocation.

## Acceptance criteria

- [ ] Discovery preserves token encoding, supply representation, ordering, limits, cursor boundaries, and empty results against the legacy schema.
- [ ] Metadata and balance queries preserve input parsing, batch behavior, response ordering, and representation.
- [ ] Version-specific whole-request failure versus omission of failed token queries matches current behavior.
- [ ] Real PostgreSQL tests use compile-time checked queries and offline metadata; controlled contract tests assert invocation inputs and failure handling.
- [ ] Reviewed fixtures cover each capability/version, malformed identifiers, partial failures, and dependency errors; CI runs all tests without production effects.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
