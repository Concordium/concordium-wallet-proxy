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

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
