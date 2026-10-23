# Block and transaction-outcome fixture provenance

Review status: source-derived expectations; independent human review pending.
No expected response is captured from replacement output. Tests do not run the
Haskell service as a comparison server or call a public node.

## Behavioral authority

`src/Proxy.hs` defines `getBlockInfoR`, `getBlocksAtHeightR`, and
`getBlockTransactionEventsR`. Hash routes parse with `readMaybe`; invalid hashes
return 400/error 1 (`Malformed block hash`). Missing blocks use 404/error 2
(`Not found`). Invalid Word64 heights miss the route (404/error 1); the height
query does not apply the custom missing-block message. Heights are absolute.
The full outcome stream is consumed before HTTP success is returned.

The legacy base sources under
`deps/concordium-client/deps/concordium-base/haskell-src/Concordium/` define:

- `Types/Queries.hs`: BlockInfo JSON fields and explicit null optional values;
  SupplementedTransactionSummary includes null sender and sponsorDetails.
- `Types/Execution.hs`: summary type/result objects, event tags, account roles,
  ordering of generated events, and the RejectReason JSON schema.
- `Types/Updates.hs` and `Types/Parameters.hs`: governance payload names, exact
  decimal/rational values, and version-specific parameter schemas.
- `Types/Tokens.hs` and `Types.hs`: token amounts, holders, raw hexadecimal CBOR
  details, and omission of absent token rejection details.
- `Common/Time.hs`: scheduled-transfer Timestamp values are numeric milliseconds,
  unlike UTCTime values in block metadata.
- `Types/ProtocolVersion.hs`: this Haskell version supports P1–P10. A P11
  block-info response therefore fails conversion rather than exposing new data.

`deps/concordium-client/src/Concordium/Client/GRPC2.hs` defines protobuf-to-event
expansion. Transfer memos follow transfer effects; encrypted removals precede
additions; account creation emits AccountCreated followed by CredentialDeployed.
Only account transactions have sender costs and optional sponsor costs. Update
and credential deployment costs are zero. Contract initialization supplies its
parameter even when the old node response defaults it to empty bytes.

## Independent expectations

`blocks.json` is hand-written from those schemas. Test-owned typed effect
builders and assertions separately encode all account-effect categories,
contract trace kinds, baker/delegation configuration tags, credential creation,
token creation, representative governance payloads, and rejection records.
Cryptographic key expectations are hex encodings of deterministic synthetic
input key bytes, not SDK summary JSON or replacement responses. Tests do not
serialize an SDK BlockItemSummary to establish an expected outcome.

Block fixtures cover slot-based and BFT protocols, genesis/no baker, max-u32
counts and sizes, max-u64 energy, heights above 2^53, explicit nulls, and exact
UTC fractional timestamps. Outcome fixtures cover maximum amount strings,
raw binary memos and parameters, ciphertexts, sponsor/sender fee attribution,
null rejection type, empty stake changes, and ordered streams whose indices
are deliberately not sorted. Scheduled-transfer releases remain numeric
millisecond/amount pairs, not SDK-formatted date strings.

Isolated Stack/GHCi checks confirmed the legacy hash Read instance accepts
trailing whitespace but rejects leading whitespace, prefixes, wrong lengths,
and trailing junk. Hash tests retain uppercase and trailing-space acceptance.
The synthetic sender/receiver address strings were independently established
through Haskell type checks in the earlier balance/staking and discovery work.

Legacy governance details are deliberately preserved:

- Payload names are `timeoutParameters`, `minBlockTime`, `blockEnergyLimit`,
  `finalizationCommitteeParameters`, `validatorScoreParameters`, and `createPLT`,
  not the newer/different SDK payload serialization names.
- Governance key updates report the key level being changed, not merely the
  signing-key level returned by the SDK's generic update_type helper. V1/V2
  authorization payloads omit unsupported SDK options and retain `createPLT`
  capitalization. GAS rewards CPV2 still has summary type `updateGASRewards`.
- Haskell's standalone FinalizationCommitteeParameters ToJSON swaps the minimum
  and maximum keys. The block update event retains that quirk. This does not
  change the chain-parameters route's different, correct flattened projection.

## Lock and future-variant policy

The Haskell client reads TokenEffect's original protobuf field-1 token-event
list. Current SDK queries prefer the newer unified list; the node contract says
its token entries duplicate the original list. Project-owned projection retains
those token entries in order and explicitly excludes lock-only entries, matching
the legacy interface even for a lock-only success with an empty event list.
Tests make this behavior explicit rather than presenting a new lock event API.

This relies on the documented equality of the node's duplicate token lists.
For a malformed response where the two lists disagree, the SDK has already
discarded the original list; exact legacy reconstruction is unavailable through
its typed result. No compatibility waiver or protobuf bypass is claimed.

Unknown SDK Upward variants, unsupported Wasm versions, lock-only rejection
reasons, and the unsupported maximum-lock-duration governance update fail the complete response rather than
leaking newer SDK schemas or returning a partial list. Haskell also cannot
decode those rejection/update variants. This is not a claim of future-protocol
compatibility.

## Dependency assertions and acceptance

Twenty new tests include three local SDK/gRPC tests. The transport tests check
Given hashes, absolute max-u64 height, sponsor preservation, empty streams,
initial missing-block status, failure after successful frames, unknown summary
variants, and a timeout during stream consumption. One uses a real HTTP listener.
All test operations are read-only against local synthetic dependencies.

Existing CI includes the new tests. Independent fixture review, Task 01
acceptance, node-cookie handling, and SDK error-provenance compatibility remain
outstanding. Some malformed protobuf conversion errors become SDK RPC statuses
instead of legacy invariant errors; original RPC NOT_FOUND messages may also be
lost. The inherited SDK/Chrono timestamp limits still apply to block metadata.
These are not waived by this increment. Controlled real-node release acceptance
and CI execution are not claimed.
