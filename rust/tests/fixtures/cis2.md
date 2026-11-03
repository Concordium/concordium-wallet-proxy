# CIS-2 fixture provenance

Status: source-derived; independent fixture and compatibility review remain
required. Task 18 implements the five existing routes:

- `GET /v0/CIS2Tokens/{index}/{subindex}`
- `GET /v0/CIS2TokenMetadata/{index}/{subindex}` and v1
- `GET /v0/CIS2TokenBalance/{index}/{subindex}/{account}` and v1

These are contract-standard CIS-2 tokens, not protocol-level tokens. Tests do not
submit transactions, fetch metadata URLs, query public nodes, or change production
data. No Haskell comparison runner or replacement-output oracle is used.

## Authority

`src/Proxy.hs:getCIS2Tokens`, `parseCIS2TokenIds`, both metadata/balance handler
versions, `retrieveInstanceInfo`, `cis2InvokeHelperError`, `getMetadataUrl`, and
`getTokenBalance` define the behavior. Its `CIS2Entry` declaration and
`CIS2TokenId` PersistField/Serialize instances distinguish raw database bytes from
length-prefixed invocation IDs. `Internationalization/En.hs` supplies error text.

The pinned base library's `Crypto/ByteStringHelpers.hs` defines raw Base16 token
IDs with no token-length check; invalid strings take Aeson's typeMismatch path.
`Utils/Serialization.hs:getMaybe` accepts only checksum tags 0 and 1. Address
parsing checks Base58 checksum, length, then version. Existing fee parsing uses
the same account and Base16 rules, now factored into shared compatibility input
helpers without changing fee behavior.

The externally owned schema is defined in the adjacent
`concordium-transaction-logger/wallet-proxy-indexer/resources/m0001-initial.sql`,
revision `6321dd1940a129b12ffe76d8423c0979e223259b` (the source file was clean).
The canonical knowledge bundle has no matching transaction-logger System document;
this SQL is schema evidence, not a new ownership decision. The test-only copy
retains BIGINT contract components, BYTEA IDs, NUMERIC(80,0) supply, the unique
contract/token key, and the contract-address index. It is not a production migration.

## Discovery evidence

`rust/tests/cis2/postgres.rs` uses isolated real PostgreSQL databases with synthetic
rows and binary IDs. It tests:

- Empty discovery without a node query; sorting by entry ID, exact index/subindex
  selection, exclusive cursors, invalid cursor defaults, and first query values.
- Default, zero, negative, hexadecimal, and capped limits, including 1005 rows.
- Empty, ordinary, and 256-byte raw token IDs rendered as lowercase hex without
  a collection-length prefix. No protocol token-size limit is added to the table.
- Exact 80-digit supply strings, negative/zero supplies, and Word64-to-BIGINT
  contract-address representation. Invalid stored numeric values and missing
  tables fail, rather than returning empty or partially decoded data.
- The external unique contract/token key and index-preserving query shape.

The production statement is compile-time checked with SQLx. The NUMERIC-to-text
cast retains database integer precision; BigInt decoding does not round through
a machine integer or floating-point number. Offline metadata is generated from
the representative schema and checked against a live database.

## Invocation and codec evidence

`rust/tests/cis2.rs` contains hand-written binary response fixtures and literal
expected HTTP fields. It covers:

- Empty token IDs, duplicates, lowercase normalization, first query value, account
  error precedence, alias bytes without canonicalization, bounded Word64 paths,
  and the negative-zero path form. v1 permits at most twenty IDs after parsing.
- All IDs are validated before the Word16 array-count guard. Counts 65535/65536
  are unit-tested directly because http::Uri cannot represent URLs that large;
  this does not pretend those inputs traversed the HTTP listener.
- Token-length prefixes wrap as Word8 while every ID byte is retained, matching
  Cereal. Unchecked receive names and parameters preserve legacy forwarding;
  a reachable balance batch verifies a 68002-byte parameter. No transaction-size
  validation is imposed on read-only invocation parameters.
- One LastFinal instance lookup, then invocation at its returned block hash.
  Every call uses no invoker, zero CCD, the prefixed contract receive name, and
  the correct entrypoint. v0 uses 500000 energy and one batch; v1 uses 50000 per
  token with sequential singleton queries at the same block.
- Metadata strings are length-prefixed UTF-8, not fetched or URL-validated. Checksums
  are nullable raw 32-byte hashes; unknown tags, truncated values, and invalid UTF-8
  fail decoding. Trailing return bytes are accepted as with Cereal runGet.
- v0 metadata decodes every declared URL, then zips with request IDs, truncating
  either side. A malformed extra URL still fails the batch. Balance counts must
  instead match the requested count. v1 requires exactly one returned item.
- Balance amounts preserve all 37 permitted LEB128 bytes, including the legacy
  259-bit maximum and overlong zero padding. They are decimal strings, not SDK
  standard-CIS-2 256-bit values or JSON floating-point numbers.
- v0 distinguishes contract failure/malformed return data from successful V0
  invocations with no return value. v1 omits those individual token results but
  never hides RPC errors, overload, timeout, missing instances, or SDK conversion
  failures. A node failure after a successful token aborts the entire response.

`rust/tests/cis2/rpc.rs` exercises the real SDK and local gRPC for V0/V1 instances
and both endpoint versions. It asserts complete protobuf selectors, method names,
energy, invoker/amount, parameter bytes, and LastFinal-to-Given block pinning.
Invocation responses deliberately carry a different block hash to prove later
calls remain pinned to the instance-info snapshot.

A real HTTP listener verifies metadata bodies, UTF-8, successful absence of
Content-Language (the legacy handlers do not internationalize successful results),
English errors, JSON charset, server/CORS headers, HEAD, methods, redirects, and
no database access on metadata/balance requests. Existing CI runs these tests and
all disposable PostgreSQL tests without a public node.

## Invocation metadata contract

The user confirmed that node invocation responses always include `blockhash`
metadata unless the block is unknown. `Client::invoke_instance` handles RPC
errors before calling `extract_metadata`; unknown-block NOT_FOUND therefore
reaches the proxy's legacy custom 404 response without requiring the header.
The local gRPC error tests return NOT_FOUND without success metadata and assert
that response.

`malformed_success_without_required_invocation_header_fails_defensively` sends
an otherwise decodable body without the required header. The SDK returns 502;
this tests malformed-node defense, not a supported successful response. The
Haskell helper does not inspect that invocation header, but this difference is
outside the user-confirmed node contract and is not a compatibility blocker.
No SDK change, protobuf bypass, fork, or user-approved compatibility exception
is required for this case. Shared cookie-forwarding and error-provenance limits
remain separate release concerns.

## Validation and acceptance

Local validation: 251 tests pass serially (five unit, 246 integration), including
fourteen disposable PostgreSQL tests. All five live SQLx descriptions match the
offline files. Package formatting, locked offline compilation, Clippy with warnings
denied, and whitespace checks pass; the SDK's two lifetime-syntax warnings remain.
CI execution and controlled real-node acceptance have not been observed.

Independent review of the schema/fixtures, exact parser wording, complete binary
behavior, Task 01 acceptance, and shared dependency limits remain required.
The invocation-header concern above does not block Task 18. No unresolved
implementation or dependency gap may be silently deferred to final release review.
