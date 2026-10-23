# Contract and token fee fixture provenance

Status: source-derived; independent review and acceptance remain outstanding.
These tests extend Task 11 with Task 12. No expectations were recorded from a
running Haskell service, and no comparison runner was added.

## Governing sources

- `src/Proxy.hs`: `getTransactionCostR`, `computeTokenOperationSpecificCost`, and
  `knownTokenOperationSpecificCostMap` define validation order, accepted names,
  invocation settings, size calculations, and buffer arithmetic.
- `deps/concordium-client/src/Concordium/Client/Types/Transaction.hs` defines the
  normal/extended minimum energy and token base energy.
- The pinned Haskell base's `Concordium/ID/Types.hs` defines checksum, length, and
  version error precedence. `Concordium/Common/Amount.hs` derives Read via Word64.
  `Concordium/Wasm.hs` accepts ASCII receive names containing a dot, up to 100
  bytes, and wraps parameter bytes without a local transaction-size check.
- The installed Stack snapshot uses Aeson 2.2.3.0. Its
  `Data/Aeson/Decoding/ByteString/Lazy.hs`, `Decoding/Conversion.hs`,
  `Types/FromJSON.hs`, and `Types/Internal.hs` define grammar diagnostics,
  first-duplicate-key behavior, bounded Int counts, error paths, and trailing
  garbage precedence. Source archive:
  https://hackage-content.haskell.org/package/aeson-2.2.3.0/aeson-2.2.3.0.tar.gz
- Scientific 0.3.8.0 defines the generic number rendering used in numeric errors:
  https://hackage-content.haskell.org/package/scientific-0.3.8.0/scientific-0.3.8.0.tar.gz
  Base16-bytestring 1.0.2.0 defines odd-length checks before invalid-byte offsets.
  Text's `UnicodeException` Show instance supplies malformed escape diagnostics.
  Source archives were inspected outside the repository; no new Haskell runtime
  dependency or generated oracle was introduced.

## Contract expectations

The standard request uses sender
`2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3`, contract 42:7, amount 123
micro-CCD, receive name `hello.go`, and parameter `00aAFF` (three bytes). The
payload is `1 + 8 + 16 + 2 + 8 + 2 + 3 = 40` bytes. With one signature and no
sponsor, minimum energy is `60 + 40 + 100 = 200`.

The node receives Best, the account invoker, the full receive name and raw bytes,
and exactly 500000 execution energy. Returned execution energy 101 receives the
default buffer `floor(101 * 20 / 100) = 20`, for total energy 321. At the mock rate
2/3, cost is the string `214`; at the SDK fixture rate 50, it is `16050`. An extra
signature and sponsorship add 134 energy, so the real HTTP/gRPC test expects 455
energy and cost `22750`. Rejection still returns HTTP 200 with `success: false`;
RPC failure does not return an estimate or a success field.

Tests cover zero/negative/overflowing buffers, multiplication wrap before integer
division, Word64 input wrapping, sender error precedence, empty and 100-byte
receive-name boundaries, strict Base16 decoding, and no invocation after invalid
input or a failed rate query. A pure preparation test retains a 65536-byte
parameter without adding a transaction-format validation rule; the HTTP test
uses a smaller parameter within the HTTP URI limit.

## Token expectations

Token payload size is the supplied signed Int `listOperationsSize`, plus the
UTF-8 byte length of `tokenId`, plus six framing bytes, converted to Word32. The
handler does not validate the identifier, operation encoding, protocol, or a
relationship between size and counts. It does not invoke a contract or submit
operations. The base operation cost is 300. Transfer costs 100 per occurrence;
the other accepted operation names use SDK constants of 50 each (see the source
discrepancy below). Counts are bounded 64-bit Int values from JSON, including
negative counts and integral decimal/exponent forms. Energy multiplication and
summation wrap as Word64.

For size 10 and identifier `CCD`, payload is 19 bytes. One occurrence of all 12
names contributes 650 energy. Total energy is `160 + 19 + 300 + 650 = 1129`, with
cost string `753` at 2/3 micro-CCD per energy.

Tests cover all accepted operation names, zero and negative counts, signed and
wrapping sizes, UTF-8/empty/long identifiers, sponsorship presence, duplicate
JSON keys (first wins), malformed/type/fraction/range errors, unknown operations
(including SDK lock operations), validation order, and typed errors before trailing
garbage. Raw JSON values avoid Serde's arbitrary-precision private-number object
representation changing the type of a supplied object. The syntax scanner uses
an explicit stack and source error literals, not Serde diagnostic-message text.

## Blockers and remaining review

- **Missing Haskell cost definitions:** Proxy.hs names
  `Cost.tokenAssignRevokeRolesCost` and `Cost.tokenUpdateTokenMetadataCost`, but
  the pinned Haskell base does not define them. The SDK supplies
  `PLT_ASSIGN_REVOKE_ROLES` and `PLT_UPDATE_TOKEN_METADATA`, both 50. The Rust
  implementation and tests use these SDK constants provisionally for
  `assignAdminRoles`, `revokeAdminRoles`, and `updateMetadata`. This does not
  establish parity with the deployed Haskell build. Confirm its exact dependency
  source and costs, or obtain an explicit compatibility decision before acceptance.
- Independent fixture review must include the full integer/JSON lexer and Show
  compatibility adapters, not only valid fee examples. If source-compatible
  syntax validation succeeds but the JSON backend cannot read the value, the
  explicit `JSON decoder compatibility limit` error remains a blocker, not an
  approved response difference. No such case is established by current fixtures.
- Node cookie forwarding and SDK response-conversion error provenance remain the
  shared release blockers. In particular malformed invocation responses can
  surface as HTTP 502 INVALID_ARGUMENT instead of a Haskell local-decoding error.
  Future/unknown SDK event variants are not independently accepted for this route.
- Task 11/Task 01 acceptance, observed CI, actual HTTP-server edge cases, and
  controlled real-node release acceptance remain outstanding. No production
  traffic, submissions, SDK modifications, or schema changes are involved.
