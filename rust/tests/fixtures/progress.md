# Submission progress fixture provenance

Status: source-derived; independent fixture and dependency-alignment review remain
outstanding. Task 15 adds `GET /v0/submissionStatus/{hash}`. All node operations in
tests use local adapters or local gRPC endpoints; no transaction is submitted.

## Governing sources

- `src/Proxy.hs`: `getSubmissionStatusR`, `getSimpleTransactionStatus`,
  `outcomeToPairs`, `outcomesToPairs`, `eventBakerId`, and the contract trace helper
  define validation, lifecycle states, conditional fields, and unsupported modes.
- `src/Internationalization/En.hs` defines rejection descriptions, including
  historical spelling and validator/delegator wording quirks.
  `Internationalization.hs` sets Content-Language even on successful lookups.
- `deps/concordium-client/src/Concordium/Client/GRPC2.hs` defines status and event
  conversion, including duplicate block hashes (last entry wins), legacy token
  event-list selection, and available rejection variants.
- The base library's `Concordium/Crypto/SHA256.hs` defines raw hexadecimal hash
  reading. `Concordium/Types/SmartContracts.hs` defines `<index, subindex>` address
  descriptions. `Concordium/Types.hs` and `Types/Tokens.hs` define token rejection
  record formatting, optional details, and token event-type byte-string Show.
- [Block outcome provenance](blocks.md) covers the shared legacy event projection.
  The block API schema is not changed. Status-specific selection and trace names
  remain separate from that projection and from SDK transaction-summary JSON.

No Haskell service, comparison runner, or replacement-output oracle was used.

## Source-derived response expectations

The reused SDK fixture has hash 0x04 repeated 32 times, sender
`2xBpaHottqhwFZURMZW4uZduQvpxNDSy46iXMYs9kceNGaPpZX`, cost `10`, and receiver
`2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3`. Tests deliberately query
hash 0xaa repeated 32 times: the legacy handler returns the summary hash rather
than substituting or validating the requested hash.

- Received and absent responses contain only `status`; only node NOT_FOUND means
  absent. Transport, overload, deadline, and conversion failures are not absence.
- Committed block hashes are sorted by raw hash bytes. Every projected outcome
  must be supported before equal versus ambiguous results are selected.
- Equal outcomes return the common details. Index and energy cost are excluded
  from equality, but hash, original sender, sender cost, sponsor presence/cost,
  memo, and transaction-specific fields are included.
- Ambiguity returns only status, blockHashes, and `outcome: ambiguous`. It must not
  leak one branch's transactionHash, sender, cost, or transfer details.
- Encrypted effects can override sender in the final JSON. Equality must still
  compare the original common sender as well as the event's sender, as Haskell's
  pair list retains both. The tests exercise this otherwise invisible difference.
- Finalized results require exactly one outcome. Empty committed or finalized
  results and inconsistent/missing summaries fail without partial information.
- Credential deployment has `sender: null`, cost string `0`, and success, but no
  account address or sponsor field. Both initial and normal credential types are
  covered.
- Amounts and costs remain decimal strings, including u64::MAX. Encrypted
  aggregatedIndex remains an exact JSON number; tests include u64::MAX.
- Sponsor absence omits the field. Presence returns `sponsor` containing its
  account and separately attributed cost, without combining sender/sponsor costs.
- Validator configuration reads an ID only from the first event. Delegation
  removal before a later validator event therefore means no bakerId field.
- Contract initialization omits its parameter. Contract update traces use the
  four lowercase types updated/transferred/interrupted/resumed and flatten
  transfer roles. Upgrade traces fail this endpoint even though the block API
  can render them; no partial trace is returned.
- A single legacy token transfer exposes token ID, raw value/decimals, receiver,
  and optional memo. Other successful token event lists return the common fields
  without an `outcome` property. Registered data remains a hexadecimal string.
- Successful scheduled transfers, legacy validator transactions, credential-key
  updates, governance updates, and token creation are unsupported simple statuses.
  Their representable account-transaction rejections still return a reject outcome.

Rejection tests use source literals for all static English cases and dynamic
module/account/contract references, method names, signed contract error codes,
and token rejection records. Typos such as `encryped` and the legacy validator
wording for AlreadyADelegator/DelegatorInCooldown are intentional. Optional CBOR
rejection details distinguish Nothing, Just empty bytes, and Just nonempty bytes.
Event types use Haskell byte-string escapes, not UTF-8 text Show or SDK Debug.
The existing fee JSON Show helper was moved unchanged into shared compatibility
code so both paths use the same quoting rules; prior fee tests remain regression
coverage.

## SDK and HTTP evidence

Local SDK tests verify exact transaction-hash request bytes, received/absent,
sponsored finalized results, equal and ambiguous committed maps, duplicate block
hashes, malformed result/summary handling, remote statuses, and bounded timeout.
Only GetBlockItemStatus is registered on these mock servers, so extra node queries
or submissions cannot silently succeed.

A real HTTP listener includes the shared middleware. It verifies successful
Content-Language en with conflicting query/cookie/header language preferences,
JSON charset, server header, optional public CORS, HEAD with an empty body,
unsupported methods, and path redirects before lookup. No database access occurs.

## Remaining acceptance limits

- Independent review is required for fixtures, complete rejection formatting, and
  event projection compatibility; local tests are not independent acceptance.
- **Meta/lock dependency alignment:** Proxy.hs contains TTMetaUpdate and newer
  optional lock fields, but the pinned Haskell gRPC adapter sets TTTokenUpdate and
  reads only the legacy field-1 token_events list. The SDK exposes a unified list;
  status conversion reuses the established legacy projection and ignores lock-only
  events just as that pinned adapter does. No meta transaction kind or lock ID is
  inferred from the unified events. Lock-related successful status branches and
  metadata in a newer deployed adapter are not claimed compatible until the
  deployed dependency revision and wire behavior are established.
- The SDK does not retain both legacy and unified token lists. As in Task 10,
  correctness relies on the node's documented duplicated token-list contract;
  deliberately conflicting lists cannot be reconstructed after SDK conversion.
- P11 rejection variants unavailable in the pinned Haskell adapter and unknown
  SDK variants fail explicitly instead of generating invented descriptions.
- The user's error-equivalence rule permits different errors when both paths
  fail. It does not relax HTTP 200 reject-outcome descriptions, successful fields,
  absent-versus-failed distinctions, or ambiguity semantics. SDK conversion-error
  provenance remains a shared concern; missing protobuf outcomes can become 502
  rather than a legacy local-decoding error, but neither yields partial success.
- Node cookie forwarding, Task 01 acceptance, observed CI, and controlled real-node
  release acceptance remain outstanding. No SDK fork or protobuf bypass is used.
