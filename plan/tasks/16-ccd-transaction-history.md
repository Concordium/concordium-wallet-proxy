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

Implemented the v0, v1, and v2 account-history routes and connected them to the
executable. LastFinal canonical-address lookup precedes filter validation; only
NOT_FOUND falls back to the supplied account. The existing checked SQL statements
retain static cursor/order variants, inclusion rules, pagination, and filters.
No schema ownership or production data is changed.

The legacy JSON decoder now renders account/credential/chain-update categories,
reward and suspension outcomes, memo-version differences, encrypted effects,
staking events, contract traces, and localized English descriptions. Sender,
receiver, alias/self-transfer, and sponsor fee effects remain distinct. Raw
rejections are presence-controlled. Invalid formats fail the whole page instead
of discarding rows. Exact integral scientific JSON decoding, full Word8 mint
exponents, single-round timestamp conversion, and Cereal block-hash prefixes are
covered. Integer parsing and rejection descriptions are shared with existing
fee/status features rather than duplicated.

Twenty-four history tests cover the storage boundary, source-derived response
fields and wording, request validation order, SDK query conversion/failures,
and real HTTP plus PostgreSQL behavior. Five use disposable PostgreSQL databases,
including an end-to-end self-transfer/sponsored-rejection/suspension fixture.
Provenance, behavior boundaries, and review status are in
`rust/tests/fixtures/history.md`.

Validation: all 222 tests pass serially (four unit and 218 integration), including
all eleven disposable PostgreSQL tests. Live SQLx re-description matches all
four offline metadata files byte-for-byte. Locked offline builds, package
formatting, Clippy with warnings denied, and whitespace checks pass. The SDK's
two existing lifetime-syntax warnings remain unchanged.

Not yet accepted: independent fixture/legacy-decoder/wording review, Task 01
acceptance, observed CI, and controlled real-node release acceptance. Acceptance
checkboxes remain open pending that evidence. No CCD route, decoder, or effect
implementation is deferred to Task 17 or Task 22. Task 17 adds token-inclusive
v3 and token/lock details; Task 22 is final release review.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
