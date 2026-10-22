# Token-inclusive balance expectations

These expectations are source-derived; independent human review is outstanding.
No expected response was captured from replacement HTTP output or a Haskell
comparison service.

## Approved compatibility decision

The user explicitly waived preservation of `_additional`. Unknown extension
properties in decoded global state, account state, and metadata are skipped
through the SDK's CBOR API and are not emitted. P11 `locks` and `available` are
therefore ignored, not exposed as new first-class fields. This waiver does not
change recognized fields, raw amount formats, omission rules, or lookup failures.
Malformed or unsupported state still uses original-hex fallback on SDK decoder
failure; this is not a requirement to redact unknown bytes from raw fallback.

The hand-written `module-state.json` cases incorporate this approved difference.
Additional tests verify that ignored extension values do not affect recognized
allow-list/deny-list properties, even when a newer SDK model assigns a typed
meaning to those extension names.

## Sources and sequencing

`src/Proxy.hs:getAccountBalanceR` adds token enrichment only for v2. It retains
v1 CCD information and emits `accountTokens: []` for an existing account with no
holdings. Missing finalized accounts still return `{}`. Account/pool queries use
the pinned finalized hash, followed by tokenomics. All token lookups complete
sequentially in original order using `LastFinal`, not the pinned account hash.
Duplicates are retained. Projection and conditional reward-period queries follow
the lookups. A failed lookup aborts the whole response without a partial list.

`Concordium/Types/Queries/Tokens.hs` in the Haskell base submodule defines entry
names, raw amount strings, numeric decimals, module references, and supply.
Absent account module-state bytes omit `state`; present empty maps become `{}`.
No metadata URL is fetched.

`Concordium/Types/ProtocolLevelTokens/CBOR.hs` defines recognized properties and
validation rules. Known optional properties are omitted when absent, not
defaulted to false/null. Invalid known property types make the entire state
value hex. Outer maps reject duplicate keys and indefinite text keys. Nested
metadata accepts indefinite text keys and resolves duplicate keys before
validation using last-wins semantics. Governance accounts use tag 40307;
optional CCD coin information uses tag 40305 and coin type 919. Account bytes
and definite SHA256 checksum bytes must each be 32 bytes.

The proxy implements those known-field JSON rules using SDK `CborDeserialize`,
map/tag decoders, and primitive values. There is no proxy-owned CBOR term reader,
term re-encoding, or direct Ciborium dependency. SDK composite JSON serializers
are not the HTTP contract. The SDK retains raw bytes, so decoder/schema failures
can render the original hex rather than losing data or returning partial state.

The global-state vector in `tests/balances/tokens.rs` comes from the SDK's
existing `token_module_state.rs` test. Expected JSON follows the Haskell schema,
including explicit `coinInfo: "CCD"`. Expected addresses use the existing SDK
primitive Base58Check format. Other cases cover multiple/empty holdings,
zero/max-u64 amounts, decimals 0/255, independent supply/holding decimals,
metadata/checksums/governance accounts, ignored extensions, invalid state,
duplicates, and failure short circuits. Two tests use local SDK gRPC transport,
one through a real HTTP listener. No public nodes, database, or transaction
submission is involved.

## Remaining acceptance limits

The SDK's configurable default nesting budget of 128 arrays/maps/tags still
applies. A decoder error at that limit follows ordinary hex fallback, not a
private HTTP 500 policy. Haskell has no corresponding explicit budget, so deep
state can render as hex rather than a decoded object. This separate difference
has not been approved by the `_additional` waiver and still requires a decision
or deeper supported decoding before full acceptance.

Fixture/projection review and existing SDK error-provenance and node-cookie
limitations remain outstanding. No full compatibility or release acceptance is
claimed.
