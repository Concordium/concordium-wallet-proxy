# Token and lock history fixture provenance

Status: source-derived; independent schema, fixture, and wording review remain
required. Task 17 adds `GET /v3/accTransactions/{address}` to the existing history
service. [CCD history provenance](history.md) covers shared query parsing,
canonical lookup, paging, error handling, and account-relative CCD effects.

## Governing sources and dependency alignment

`src/Proxy.hs:getAccountTransactionsV3R`, `getAccountTransactionsWorker`,
`formatEntry`, `renderTransactionType`, and `eventSubtotal` define v3 inclusion,
conditional token/lock fields, sponsors, and CCD totals. Token balances are not
added to public CCD subtotals. `src/Internationalization/En.hs` defines the event
and rejection descriptions, including the historical `LockReturnNotAuthorized`
name and wording. `Concordium.Client.Utils.tokenAmountToString` defines display
arithmetic; `Config.showPrettyJSON` uses Aeson pretty JSON for token creation.

The current client pin `2b9c73be6794382477d054a67cb0d7f29eb18d07` and base pin
`4bd2edced1f37f295be7e866d274d97fcff6eb5b` do not define the newer meta/lock
fields referenced by Proxy.hs. The original lock-support commits and committed
fixtures provide a closed local source for the missing historical definitions:

- Wallet proxy `89c239d` introduced meta updates, lock details, and the three
  fixtures under `test/fixtures/meta-update-*.json`. It references client
  `cd087e6632905be913969eb9dd14db310870cc0f`.
- Wallet proxy `17d5478` references client
  `0af60d5f23e79749104234ca73b9df2c42f8d8be`, which references base
  `6e51a16d65468dd0804c32e6aacf92dbb6b5cefc`. This is the revision with the
  compatible historical lock rejection names used by the current proxy source.
- In that base revision, `Types/Execution.hs` defines meta/token types, optional
  transfer lock fields, lock events, and lock rejections. `Types/Locks.hs` defines
  the three-Word64 LockId JSON object and version-3 Base58Check/ULEB128 Show.
  `Types.hs` defines account-only TokenHolder JSON/Show, CreatePLT, and token
  rejection JSON with omitted absent details. `Types/Tokens.hs` defines decimal
  amounts and event-type byte limits. Lock configuration is opaque RawCbor.
- The same client revision's `Utils.hs` retains the Word64 display arithmetic.
  These sources were read directly from local Git objects, not obtained by
  executing a Haskell comparison service or copying replacement output.

The selected SDK already supplies matching LockId, TokenAmount, TokenHolder,
TokenId, RawCbor, and CreatePlt field types. Meta-update transaction names and
historical return rejections are decoded as persisted legacy JSON, not inferred
from current SDK transaction kinds. The SDK's newer release/duration rejection
names are not aliases for historical return reasons and are not accepted as such.

No SDK fork, protobuf bypass, Haskell source edit, or dependency-pin change is
introduced. This resolves the stored-JSON history boundary; it does not establish
which Haskell client revision is deployed or change Task 15's node-wire status
conversion limits. The baseline pin mismatch remains a release-evidence concern.

## Synthetic response evidence

`rust/tests/history/token_events.rs` reads the three committed Haskell fixtures
verbatim. Literal expected lock text uses independently derived version-3
Base58Check identifiers: `WV6MDZDimp`, `WsA9qh35Yb`, `XFDxZx4Hoa`, and `XdHmDZ3Ff9`.
The full Word64 boundary identifier is checked separately. Expectations do not
come from the Rust response or SDK LockId Display implementation.

The tests retain:

- Token-update and historical meta-update labels, token creation/governance,
  module events, mint/burn events, account holders, and unchanged CCD behavior.
- Exact singleton-pattern rules. Only a single token transfer adds holder/amount
  fields; only singleton meta lock events add lock details. Mixed/empty event
  lists retain their event descriptions without inventing transaction details.
- All combinations of optional source/destination locks and memo. Missing and
  null values omit fields and text; present values retain normalized IDs/hex.
- Raw token value strings, precision, fixed decimals, leading-zero normalization,
  unsigned LockId JSON numbers, and integral scientific input normalization.
- Legacy Word64 decimal-divisor wraparound: decimals=20 at maximum value renders
  `2.02914184810805067775`, not an idealized arbitrary-precision decimal. At 64 or
  more decimals the legacy divisor becomes zero, so the replacement fails the
  selected page instead of reporting fabricated success. Error status equivalence
  is not claimed for the legacy runtime exception.
- Opaque lock configuration and module CBOR bytes, including syntactically valid
  hex that is not valid CBOR. No config schema or governance meaning is inferred.
- Token ID and event-type byte limits, byte-string Show escapes, four-space sorted
  token-creation pretty JSON, unknown tags, bad holders/IDs/hex, and malformed
  amount/lock fields. Invalid selected data fails the whole page without dropping
  transactions or returning a partial page.

`token_rejections.rs` covers all historical lock reasons, token-ID and module
rejections, sponsor-attributed costs, and presence-controlled raw reasons. Absent
module details are omitted from raw JSON, not serialized as null; empty bytes
remain distinct. New SDK release/duration variants fail rather than being guessed.
Shared submission-status rejection behavior is unchanged.

## PostgreSQL, HTTP, and validation

`token_postgres.rs` serves mixed CCD, token, creation/governance, sponsored, lock,
reward, and rejected rows through real PostgreSQL, local SDK/gRPC canonical
lookup, and a real HTTP listener. It verifies the three Haskell lock fixtures,
old-version exclusion, memo behavior, cursor order, time/height bounds, reward
and encrypted filters, zero limits, language precedence, headers/CORS, HEAD,
methods/redirects, query failure, and decoding only the selected page.

The existing PostgreSQL filter matrix now includes v3. v0/v1/v2 continue to exclude
`tokenUpdate`, `metaUpdate`, and `updateCreatePLT`; v3 retains those rows without
changing reward or suspension selection. Both checked static statements have
regenerated SQLx metadata. All four live descriptions match the offline files.
The external schema and its ownership are unchanged. No public node, transaction
submission, or production database is used; existing CI includes these tests.

Local validation: 234 tests pass serially (four unit, 230 integration), including
all twelve disposable PostgreSQL tests. Clippy with warnings denied, package
formatting, offline compilation, and whitespace checks pass. The SDK retains its
two pre-existing lifetime-syntax warnings. CI has not been observed.

## Acceptance limits

Independent review of these schema references, response fixtures, complete
rejection/pretty formatting, and the preserved decimal arithmetic is required.
Task 16/Task 01 acceptance, baseline/deployed dependency alignment, observed CI,
and controlled real-node release evidence remain outstanding. No unknown persisted
format is silently discarded or claimed compatible. Additional supported formats
without an authoritative schema must block acceptance until resolved or explicitly
excepted; they must not be deferred to the final release-review task.
