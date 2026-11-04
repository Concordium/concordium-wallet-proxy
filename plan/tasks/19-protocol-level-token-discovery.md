# 19: Discover protocol-level tokens and retrieve token information

- **Type:** AFK
- **PRD coverage:** UC 17, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver the existing protocol-level token list and individual token-information capabilities using the SDK and explicit compatibility representations.

## Acceptance criteria

- [ ] Token identifiers, decimals, supply, module references, module state, and metadata references match current behavior.
- [ ] Empty lists, malformed or unknown identifiers, applicable protocol differences, and node failures preserve the current contract.
- [ ] Reviewed fixtures cover representative token configurations and optional state, boundary quantities, and failure responses.
- [ ] Controlled adapter tests verify relevant node operations and response conversion; CI requires deterministic tests.
- [ ] SDK serialization is used only when tests establish compatibility; missing support blocks completion.

## Implementation status

Implemented `/v0/plt/tokens` and `/v0/plt/tokenInfo/{id}` in `rust/src/plt.rs`,
with the production SDK list adapter in `rust/src/node/plt.rs` and startup route
registration. List queries preserve node order and duplicates, query each token
sequentially at LastFinal, and fail without partial output. Individual queries
also select LastFinal. The explicit global information conversion is shared with
account balances, including legacy CBOR-to-object/hex behavior; no SDK composite
serialization is adopted as the HTTP contract.

Malformed identifiers retain the legacy caught-client-error HTTP 502 rather
than introducing a 400 correction. Provenance and review gaps are recorded in
`rust/tests/fixtures/plt.md`. Eight deterministic tests cover handler behavior
and local SDK gRPC list/information operations, stream failures, and malformed
node output. Existing balance token tests cover the shared module-state converter.

Validation: `TMPDIR=/tmp SQLX_OFFLINE=true cargo test --locked -q` passed
(5 unit tests and 240 integration tests; 14 database/live tests ignored).
`SQLX_OFFLINE=true cargo clippy --locked --all-targets -- -D warnings` passed;
the SDK still emits its two existing lifetime-syntax warnings. The initial full
test invocation failed because the inherited TMPDIR did not exist; using `/tmp`
resolved those environment-only failures.

Not yet accepted: independent fixture and identifier-parser review, Task 01
acceptance, observed CI, baseline/deployed dependency alignment, and controlled
real-node protocol acceptance remain open. No SDK or dependency pin was changed.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
