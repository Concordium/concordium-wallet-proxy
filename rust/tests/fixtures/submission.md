# Credential and standard transaction submission provenance

Status: source-derived fixtures; independent review remains outstanding.
Task 13 covers `PUT /v0/submitCredential` and `PUT /v0/submitTransfer` only.
Extended/sponsored and raw submission routes remain Task 14.

## Approved error equivalence

The user approved changed errors when a request results in an error in both
implementations. This permits SDK-local rejection of malformed proof bytes that
Haskell forwards for node rejection. Tests require an error and no successful
identifier; they do not require identical error text or classification for such
inputs. This does not waive successful input interpretation, submission contents,
identifiers, or retry behavior. This decision is recorded in Task 13; it is not a
global change to other tasks' contracts.

## Sources and independent byte expectations

- `src/Proxy.hs`: `putCredentialR`, `putTransferR`, and `doSendBlockItem` define
  envelopes, credential version 0, local identifier calculation, and node-failure
  rejection. The node's returned hash is not the wallet's submission identifier.
- `deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Types/Transactions.hs`
  defines signature framing, signature/header/payload decoding, opaque payloads,
  and bare-block-item tags. Account submissions serialize the JSON signatures,
  append the decoded transaction body, then deserialize that combination.
- The base library's `Concordium/ID/Types.hs` defines initial/normal credentials,
  public keys, policies, revokers, and opaque proof bytes. `Common/Time.hs`
  defines the two-byte year plus one-byte month representation.
- `deps/concordium-client/src/Concordium/Client/GRPC2.hs`, `withGRPCCore`, retries
  an operation once after transport failure and successful reconnection, but not
  remote RPC rejection or timeout. Rust reuses the existing `SdkNode::query`
  reconnection policy instead of adding a submission retry loop.
- Attoparsec-aeson 2.2.2.0's `Data/Aeson/Parser/Internal.hs` documents first-key
  duplicate handling for `json'`; parsing one value does not require EOF. Aeson
  2.2.3.0's `Internal/Scientific.hs` and `Types/FromJSON.hs` define integral
  scientific values and bounded numeric map indices.

`submission/fixtures.rs` constructs expected bytes directly from these layouts,
not SDK serialization. Golden hashes were calculated independently with Python
`hashlib.sha256` over the source-defined bytes. No Haskell runner was used.

| Input | Bare item size | SHA-256 submission identifier |
| --- | ---: | --- |
| `examples/initial-account.json` | 238 | `67fde09681c15241f40a045f31afe5659318d5f87044d15240b682d48de0851a` |
| `examples/cdi.json` | 3404 | `af8d639191fa6194c0ff285bcedb1f47b8b05ca477aae68dae26e1c524e06888` |
| Synthetic standard transaction | 308 | `cf836d6b87d0bcb141d85e73b1b33814871253843044509fd14b2ea4478e6768` |

The normal credential identifier also matches the existing README example.
The synthetic account transaction uses sender bytes 0x01 repeated 32 times,
nonce and expiry u64::MAX, energy 45000, and a 41-byte simple-transfer payload
(tag 3, recipient bytes 0x02 repeated 32 times, amount 999 micro-CCD). Signatures
are credential 0/key 0: 64 bytes 0xab; credential 0/key 2: 64 bytes 0xcd; and
credential 3/key 1: 64 bytes 0xef. These are framing fixtures, not valid signatures.

## Exercised behavior

HTTP handler tests compare every serialized block-item byte for both credential
types and the standard transaction. They check unsupported credential versions,
locally rejected malformed proofs, signature maps and truncated bodies, opaque
account payloads, trailing binary/JSON input, first duplicate keys, integral
scientific values and indexed keys, node rejection, and absence of identifiers
on failure. Expired historical fixture credentials are intentionally forwarded;
the proxy does not invent freshness or signature verification before submission.

Local gRPC tests inspect every credential payload byte and expiry, and every
standard transaction signature, header field, and raw payload. The node returns
a deliberately unrelated 32-byte hash to prove the wallet identifier is computed
locally. A real HTTP listener also verifies submission without a JSON content-type
requirement. RPC rejection is never retried, even when connection retries are
configured. A real response-stream reset verifies identical resubmission after
one reconnect, and that a second reset does not cause a third send. Timeout tests
count arriving RPCs to ensure a timeout is not replayed.

All operations go to in-process adapters or local mock endpoints, never a public
node. No transaction is signed, production effect is created, or database queried.

## Remaining acceptance limits

- **Malformed node hash:** Haskell treats a successful gRPC response as accepted
  even if its returned hash cannot be decoded. The SDK combines local hash
  conversion errors with remote INVALID_ARGUMENT statuses. Rust consequently
  returns an error for an OK response carrying a malformed hash. This is tested
  explicitly and remains a shared SDK error-provenance blocker. The user's
  error-equivalence decision does not waive this success-versus-error difference;
  no message-text heuristic or protobuf bypass is used.
- JSON normalization has a 128-level nesting bound. Successful legacy requests
  with deeper ignored fields have not been established compatible; full HTTP
  resource-limit and lexical review remain required. SDK typed credential
  normalization beyond the two example layouts still needs independent review.
- Node cookie forwarding, Task 01 acceptance, observed CI, and controlled real-node
  release acceptance remain outstanding. Mock acceptance proves submitted
  contents and HTTP behavior, not actual credential validity or node acceptance.
