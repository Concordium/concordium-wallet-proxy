# Extended and raw submission provenance

Status: source-derived fixtures; independent review remains outstanding.
Task 14 adds `PUT /v0/submitExtended` and `PUT /v0/submitRawTransaction` to the
[Task 13 submission path](submission.md). Tests use mocks, never a public node.

## Compatibility decision

The user's error-equivalence rule is carried forward: when both implementations
return an error, exact error wording or classification may differ. This permits
SDK-local rejection of malformed binary proofs that Haskell forwards for node
rejection. Successful contents, identifiers, and retry behavior are not waived.
An error must not be reported as successful submission. No alternate sponsorship
model, private SDK fork, or raw protobuf submission bypass is introduced.

## Sources and acceptance boundary

- `src/Proxy.hs`, `putExtendedR`, serializes `TransactionSignaturesV1` from JSON,
  appends the Base16 transaction body, and deserializes an account transaction V1.
  Its envelope uses `signatures.sender` and optional `signatures.sponsor` maps.
- `putRawTransactionR` uses `getBareBlockItem SP10` on bare bytes. It accepts
  block-item tags 0 (standard account transaction), 1 (credential deployment),
  2 (governance update), and 3 (extended account transaction). No extra version
  prefix, JSON decoding, or Base16 transformation is applied to this route.
- `deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Types/Transactions.hs`
  defines extended signature framing, the two-byte header bitmap, sponsor address,
  item tags, and the hardcoded protocol gate. Account payloads remain opaque;
  P10 governance restrictions are not applied to account payload operation tags.
- `Concordium/Types/Updates.hs` defines P10 governance payload gates, isolated
  payload lengths, key-update authorization versions, and serialization after
  decoding. P10 uses chain parameters version 3 and authorizations version 2.
- SDK `AccountTransactionV1` and `UpdatePayload` decoders supply supported typed
  parsing; `v2::Client::send_block_item` supplies submission. Although the SDK's
  generic `BlockItem` decoder omits tag 3, its public V1 decoder, block-item variant,
  and submission conversion support it. The adapter reads the discriminant and
  uses those official types without changing SDK code or using protobuf directly.

Raw governance accepts payload tags 1, 3, 4, 5, 7, and 12 through 24. Root-key
updates (tag 10) permit variants 0, 1, and 4; level-1 updates (tag 11) permit
variants 0 and 3. Old CPV0 variants, old level-2 authorization layouts, and the
P11 maximum-lock-duration update are rejected even if the SDK can decode them.
The entire isolated payload must be consumed. The decoded payload is serialized
back through the SDK, with the original header retained, as Haskell does.
Outer trailing bytes are ignored and excluded from both submission and hashing.

Sponsor/header consistency and cryptographic signature validity remain node
checks. In particular an empty sponsor map serializes as the same zero-byte
sentinel as absent or null sponsor signatures; the existing deserialize step
therefore converts it to no sponsor signatures. No corrective rule is added.

## Independent byte fixtures

`submission/fixtures.rs` builds the expected bytes directly from the source
layout, not SDK serialization. Golden SHA-256 values were derived independently
with Python `hashlib` over that layout. Existing Task 13 credentials and standard
transaction fixtures are also exercised through raw submission.

| New fixture | Bare item size | Submission identifier |
| --- | ---: | --- |
| Extended, no sponsor | 311 | `734777b97794b0706a62676c9288953189f4f726d35c5b65f006b43054ecfae4` |
| Extended, sponsored | 412 | `f5de8424f8e8629aef626a77cdfc87902c104a74b84301bb8c9e06fcd6bbbb85` |
| EUR-per-energy update | 116 | `99cfc039ad665a2ac2ee8fe2645b09b80c2eb2735769106e7ebc26618adc23a1` |

Extended fixtures reuse the source-defined Task 13 sender header, signatures,
and simple-transfer payload. Sponsorship adds address bytes 0x04 repeated 32
times and credential 2/key 7's 64-byte signature of 0x12. The header bitmap is
0x0001 when sponsored, otherwise 0x0000. Sender signatures remain unchanged.
The submission identifier hashes the entire bare item, including tag 3; the V1
signing-domain prefix is not a prefix of the submission-identifier preimage.

The governance fixture has sequence 5, effective time 0, timeout u64::MAX, and
payload tag 3 with numerator 2 and denominator 3 (17 payload bytes). Its one
signature uses update key 7 and 64 bytes of 0x34. Additional source-built root
and level-1 key-update fixtures test accepted higher-level key layouts. A valid
old election-difficulty payload and valid P11 maximum-lock-duration payload are
shown to decode in the SDK but still fail the legacy P10 gate.

## Tests and safety

Seventeen new integration tests cover:

- Exact extended bytes and identifiers, sender/sponsor signatures, all four
  sponsor/header presence combinations, absent/null/empty sponsor maps, numeric
  key aliases, and duplicate/trailing input.
- All four raw kinds, both credential forms, extended items with and without
  sponsorship, governance updates, unknown tags, incomplete input, unknown bitmap
  bits, noncanonical signature order, oversized lengths, empty signature maps,
  and unused bytes inside versus outside an isolated governance payload.
- Complete local SDK/gRPC request fields. Raw and JSON routes produce equivalent
  protobuf operations for the same supported item. The node's unrelated response
  hash never replaces the locally computed identifier.
- Rejection and timeout without replay, transport reset with one identical replay,
  and rejection after a second reset without a third send. The existing transport
  policy is reused; no new signing, nonce query, retry loop, or node query is added.
- A real HTTP listener around the shared middleware: JSON charset, server header,
  error language, optional public CORS/preflight, method rejection, redirects
  before submission, and acceptance without requiring a JSON content type.

No test signature is used against a real chain. Mock acceptance is evidence for
framing and forwarding, not cryptographic validity or real-node acceptance.

## Remaining review and blockers

Independent fixture review must cover the remaining P10 governance payload and
authorization variants, SDK serialization compatibility, and complete binary
edge cases. Task 13/Task 01 acceptance, observed CI, and controlled real-node
release acceptance remain outstanding.

The shared node-cookie and malformed-node-hash provenance limits still apply.
In particular an OK node reply with an undecodable hash can make the SDK return
an error where Haskell returns the local identifier. That success-versus-error
difference is not waived by error equivalence. JSON nesting/server resource-limit
review from Task 13 also remains relevant to the extended JSON envelope.
