# Account-history fixture provenance

Status: source-derived; independent fixture and decoder review remain outstanding.
Task 16 serves `GET /v0/accTransactions/{address}`, v1, and v2. Token-inclusive
v3 and token/lock rendering belong to Task 17, which depends on these CCD routes.
No Haskell service, comparison runner, or replacement-output oracle is used.

## Authority

- `src/Proxy.hs`: `getAccountTransactionsWorker`, `formatEntry`, `eventSubtotal`,
  `sameAccount`, `renderTransactionSummaryType`, and the memo/version selectors
  define query order, filtering, response fields, fee attribution, and effects.
- Its `Summary sql=summaries` and `Entry sql=ati` declarations define the existing
  join, BYTEA identifiers, BIGINT fields, JSONB envelope, and entry cursor.
- `src/Internationalization/En.hs` and `Base.hs` define the English descriptions,
  six-digit CCD display, contract/account notation, multiline reward text, and
  commission formatting. English is the only supported response language.
- The pinned Haskell base library's `Types/Queries.hs` defines the supplemented
  summary record. `Types/Execution.hs` defines event/result/type decoding,
  including the missing contract-version default and supplemented initialization
  parameter. `Types/Transactions.hs` defines special outcomes and reward maps.
- `Common/Amount.hs` and `Types.hs` define wrapping Amount arithmetic, normalized
  MintRate bounds (Word32 coefficient, Word8 exponent), and fraction Show.
- Persistent derives SQL-key PathPiece from Int64. `Web.PathPieces` uses bounded
  signed decimal parsing, unlike Read-based modular query values. The upstream
  `yesodweb/path-pieces/Web/PathPieces.hs` source was inspected for this distinction.
- [Submission progress provenance](progress.md) covers the shared English reject
  descriptions. Those descriptions and the Read integer helper now live in shared
  compatibility modules; existing status and fee tests remain regression coverage.

## Synthetic storage evidence

`rust/tests/history.rs` loads the existing test-only schema into isolated real
PostgreSQL databases. Its minimal envelopes are hand-written from the Haskell
query's JSON paths and deliberately do not require successful detail decoding.

The tests cover empty tables, account isolation, exact canonical account bytes,
entry IDs opposite to summary IDs, ascending/descending exclusive cursors,
inclusive time/height bounds, intersected filters, zero/default/capped limits,
and duplicate entries for a summary. They cover all reward modes and separate
reward exclusions, encrypted-only intersections, v0/v1 suspension exclusion, v2
suspension inclusion, and all pre-token PLT exclusions.

Missing/null JSON fields retain PostgreSQL three-valued logic. `Left` key presence
is not truthiness, and missing reward tags do not pass `<>` comparisons. Signed
BIGINT values retain the Word64 bits. Block hashes require at least 32 bytes and
retain Cereal's acceptance of trailing bytes. Bad records fail without a partial
page; missing tables remain database query failures, not empty results.

Both static statements are checked with SQLx and have offline metadata. Live
PostgreSQL re-description was byte-compared with all four existing metadata files.
No production schema, unchecked dynamic query, or SQLx exception was introduced.

## HTTP and rendering evidence

`rust/tests/history/{http,rendering,legacy,boundaries,rpc}.rs` use hand-written
legacy envelopes, source-defined crypto fixtures, and literal expected fields/text.
They do not obtain expected responses from the Rust summary converter.

- Invalid addresses fail before node access. LastFinal canonical lookup precedes
  filter validation. Only node NOT_FOUND falls back to the supplied address;
  transport, conversion, overload, and deadline failures never become absence.
- Query duplicates retain their first value. Read syntax includes hexadecimal,
  octal, parentheses, whitespace, and wrapping integers. Cursor parsing instead
  checks signed Int64 bounds; invalid cursors are omitted. Filter errors retain
  their legacy precedence and singular `blockReward`/`bakingReward` wording.
- All pre-token account types, both credential types, and pre-token chain-update
  labels are exercised. Unknown formats and missing required fields fail the
  entire response with a database decoding error rather than dropping records.
- v0 strips memo events/fields and maps the response type, but preserves the
  original memo transaction description. Memos are attached only for the exact
  remaining singleton-event pattern.
- Sender, receiver, alias/self-transfer, sponsor, and same-sender/sponsor cases
  retain distinct origin/subtotal/cost/total behavior. Amounts and signed totals
  are decimal strings; energy and heights retain full unsigned JSON precision.
- Scheduled amounts wrap as Amount before conversion to signed subtotals.
  Rejections omit subtotal/events, retain English reject wording, and include
  normalized raw reasons whenever the query parameter is present, even with `n`.
- Transfers, all encrypted effects, contract initialization/update/upgrade traces,
  credential/key events, registered data, and validator/delegation events retain
  event text, conditional detail fields, and public/encrypted balance effects.
  Historical missing contract versions default to V0; null versions fail.
- Every special reward type and suspension outcome has a synthetic case. Reward
  maps use exact canonical keys, sort by raw address, and retain the last duplicate.
  A missing reward-map account yields `total: null`, not zero. Accruals, pool
  rewards, and suspension totals retain the legacy zero Amount string.
- Integral scientific JSON numbers are normalized exactly before typed decoding;
  fractional/overflowing integer fields fail. Mint-rate validation preserves the
  full legacy exponent range without rust_decimal truncation. Block time rounds
  the exact rational to Double once, including a double-rounding boundary case.
- Real local gRPC tests assert the account bytes and LastFinal selector and cover
  timeout, malformed responses, remote errors, and NOT_FOUND. No submission RPC
  or public node is used.
- A real HTTP listener joins real PostgreSQL rows after SDK alias resolution. It
  covers self-transfers, sponsored rejections, suspension version boundaries,
  raw reasons, paging, malformed-summary 502 responses, missing-table 500 responses,
  language precedence, charset/server/CORS headers, HEAD, methods, and redirects.

Existing CI runs these tests with `--include-ignored` and a disposable PostgreSQL
service. Local validation passes; a CI execution has not been observed.

## Remaining acceptance

Independent review of the fixtures, complete legacy decoding, and wording is
required before compatibility acceptance. Task 01 and its shared node-cookie and
SDK error-provenance limits remain release concerns. Controlled real-node release
acceptance remains required; no production data or chain state was changed.

Task 17 must add v3 token-inclusive selection and token/lock details. These CCD
routes deliberately do not infer newer event variants or silently discard them.
Task 22 is final release review, not a deferred CCD implementation phase.
