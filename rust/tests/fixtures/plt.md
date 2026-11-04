# Protocol-level token discovery fixtures

## Source and review

Expected HTTP values in `rust/tests/plt.rs` are source-derived, not captured from
replacement output. Independent human review is pending.

Authorities:

- `src/Proxy.hs`: `getPltTokensR` calls `getTokenList LastFinal`, then calls
  `getTokenInfo ... LastFinal` sequentially for every returned identifier.
  It preserves order and duplicates and fails the whole request on any failure.
  `getPltTokenInfoR` uses `getTokenInfoFromText ... LastFinal`.
- `deps/concordium-client/src/Concordium/Client/GRPC2.hs`:
  `getTokenInfoFromText` calls `logFatal` for malformed identifiers.
- `deps/concordium-client/src/Concordium/Client/Cli.hs`: `logFatal` throws
  `ExitFailure`. `runGRPCWithCustomError` in `src/Proxy.hs` catches
  `SomeException`, classifies this as a client error, and sends HTTP 502 with
  `EMGRPCError`. This is deliberately not corrected to HTTP 400.
- `deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Types/Queries/Tokens.hs`:
  `TokenInfo` and `TokenState` select the explicit global fields. Module state
  decodes as a token-module object or falls back to lowercase hexadecimal.
- `src/Internationalization/En.hs`: generic node status messages include
  `GRPC call failed: `; malformed identifiers use the transport/client message.

## Coverage

`rust/tests/plt.rs` covers empty lists, ordered/duplicate identifiers, individual
lookup, malformed identifiers without node access, unknown tokens, list-query
failure, and early termination on an information failure. Global values include
maximum u64 supply as decimal text, maximum decimals, empty CBOR map, and invalid
CBOR as hex. The information converter is shared with account balances; existing
`balances::tokens` tests exercise metadata, optional module properties, precision,
CBOR boundaries, and fallback behavior through that same converter.

`rust/tests/plt/rpc.rs` uses local gRPC transport with the production SDK adapter.
It checks the last-final selector for list and individual information requests,
ordered stream collection, malformed identifiers in the node stream, and stream
failure after a valid entry. No public node or production writes are used.

## Remaining acceptance

Independent fixture/parser review, observed CI, Task 01 acceptance, and shared
baseline/deployed dependency alignment remain open. Exact Haskell-versus-SDK
identifier lexer equivalence is not established by the representative malformed
cases. Unsupported protocol/node statuses use the shared generic error policy;
controlled real-node protocol acceptance remains a release requirement.
