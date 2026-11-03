# 16: Browse CCD transaction history across historical versions

- **Type:** AFK
- **PRD coverage:** UC 10–11, 23; CCD and historical versions

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver all pre-token-inclusive account-history versions against existing PostgreSQL records, from filtering and paging through legacy summary decoding and localized transaction details. Include rewards, memos, encrypted activity, staking, contract activity, validator suspension, and sponsorship as applicable.

## Acceptance criteria

- [ ] Version-specific inclusion, memo treatment, descriptions, event details, fee attribution, and public/encrypted balance effects match current behavior.
- [ ] Ordering, cursor boundaries, default and capped limits, time/height filters, reward filters, encrypted-only selection, and raw rejection options are compatible.
- [ ] Queries use compile-time checked SQLx statements with offline metadata; prefer static filter and ordering variants. Dynamic unchecked exceptions require approval.
- [ ] Real PostgreSQL tests use synthetic legacy JSON and binary identifiers, including empty pages, boundary filters, self-transfers, sponsors, and rejected transactions.
- [ ] Independently reviewed HTTP fixtures assert body and metadata, validation order, localized wording, and database failures; tests run in CI.
- [ ] Existing schema ownership remains unchanged; legacy decoding gaps block completion.

## Implementation status

In progress; storage selection only. Added a typed account-history read interface
and two compile-time checked SQLx statements for ascending/descending entry
cursors, inclusive time/height bounds, capped limits, reward filters,
encrypted-only selection, and pre-token version exclusions. The adapter reads
existing `ati`/`summaries` rows without schema changes, preserves legacy JSON,
and decodes binary block identifiers. Offline metadata is included alongside
the statements.

Four real PostgreSQL integration tests cover the storage boundary, including
account isolation, ordering, boundary filters, duplicate summary entries,
version/filter interactions, JSON null semantics, signed Word64 storage, and
query/record failures. Provenance and limits are in
`rust/tests/fixtures/history.md`. These are not accepted HTTP fixtures.

Validation: all 202 tests pass serially, including the ten disposable PostgreSQL
tests (four new history tests and six existing database tests). Both statements
were checked against live PostgreSQL; offline builds, package formatting,
Clippy with warnings denied, and whitespace checks pass. Existing SDK
lifetime-syntax warnings remain unchanged. CI has not been observed.

Still required: canonical-address lookup, query parsing and validation order,
complete legacy summary decoding, localized details/events, fee attribution and
balance effects (including self-transfers and sponsorship), HTTP routes and
reviewed HTTP fixtures. No account-history route is exposed yet. Acceptance
checkboxes remain open; this task is not complete.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
