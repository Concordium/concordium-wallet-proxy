# Account-history selection evidence

## Authority and scope

`src/Proxy.hs:getAccountTransactionsWorker` is the source for the two checked
statements in `rust/src/database/history_{ascending,descending}.sql`.
The `Summary sql=summaries` and `Entry sql=ati` declarations define the join,
BYTEA account/block identifiers, BIGINT fields, JSONB summary, and entry cursor.
No production schema changes are introduced.

This is storage-layer evidence only. It is **not** an HTTP fixture catalogue,
legacy summary decoder, or acceptance of Task 16.

## Synthetic cases

`rust/tests/history.rs` loads the existing test-only schema into isolated real
PostgreSQL databases. Its envelopes are hand-written from the Haskell query's
JSON key paths, not copied from Rust responses. Most are deliberately incomplete
summaries: these tests must exercise database selection independently of decoding.

The tests cover:

- Empty tables, account isolation, and exact canonical account bytes. Alias
  resolution must happen before the query; it is not approximated in SQL.
- Entry identifiers that run opposite to summary identifiers, ascending and
  descending order, exclusive cursors, inclusive time/height boundaries,
  intersected bounds, zero/default/capped limits, and duplicate summary entries.
- v0/v1 suspension exclusion, v2 suspension inclusion, and PLT exclusion in every
  pre-token version, including `updateCreatePLT`, `tokenUpdate`, and `metaUpdate`.
- All reward-selection modes, separate block/finalization/baking exclusions,
  their intersections with encrypted-only selection, and all four encrypted types.
- PostgreSQL three-valued logic for missing and null JSON fields. In particular,
  `Left` key presence must not become a truthiness test, and missing reward tags
  do not pass `<>` comparisons.
- The signed BIGINT representation of Word64 timestamps/heights, unchanged JSON
  envelopes, binary block decoding, failure after valid rows, and query failures.

## Validation and remaining work

SQLx checks both static statements against PostgreSQL and emits committed offline
metadata. The integration tests are marked as requiring a disposable database;
existing CI runs them with `--include-ignored`.

Independent review remains required. Task 16 still needs canonical account
resolution, query parsing and validation order, full persisted-summary decoding,
localized detail/event rendering, fee and balance effects, v0/v1/v2 HTTP routes,
and reviewed HTTP body/metadata/error fixtures. Storage selection must not be
presented as successful end-to-end history support.
