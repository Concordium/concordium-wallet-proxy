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

## Implementation status

Implemented all five existing discovery, metadata, and balance routes and wired
these into the executable. Discovery reads the external CIS-2 table through a
checked SQLx statement with regenerated offline metadata. Raw token encoding,
80-digit supplies, ordering, limits, cursors, and signed Word64 storage are
covered without a schema migration.

Metadata/balance reads use one LastFinal instance lookup, then SDK-backed
invocations pinned to its response block. v0 sends one 500000-energy batch;
v1 sends sequential 50000-energy singleton calls, omitting contract failures
but aborting on node failures. The custom legacy codecs retain metadata zip
truncation, exact balance counts, strict UTF-8/checksum tags, trailing return
bytes, 37-byte amounts, and unchecked token/parameter length behavior. Successful
language-header absence and existing error wording are retained.

Seventeen new tests (one unit and sixteen integration) cover source-derived
response fields, binary/validation boundaries, real PostgreSQL discovery, real
SDK/gRPC inputs and failures, and actual HTTP metadata/method/CORS behavior.
The test-only external schema preserves NUMERIC(80,0), BYTEA identifiers, the
contract/token uniqueness constraint, and the contract-address index. Existing
fee parsing uses the factored compatibility helpers without behavioral changes.

Validation: 251 tests pass serially (five unit and 246 integration), including all
fourteen disposable PostgreSQL tests. All five SQLx descriptions match live
PostgreSQL metadata. Locked offline compilation, package formatting, Clippy with
warnings denied, and whitespace checks pass. The SDK's two existing lifetime
warnings remain unchanged. Provenance and limits are in `rust/tests/fixtures/cis2.md`.

The user confirmed the node contract: invocation responses include blockhash
metadata unless the block is unknown. Unknown-block RPC errors are handled by
the SDK before metadata extraction and retain the legacy custom NOT_FOUND
response. A successful response without the header is outside this contract;
the defensive malformed-response test is not a compatibility blocker. No SDK
change or compatibility exception is required for this case.

Not yet accepted: independent schema/fixture/parser/codec review, Task 01
acceptance, shared cookie/error-provenance limits, observed CI, and real-node
release evidence. No implementation is deferred to Task 22; acceptance
checkboxes remain open pending that evidence.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
