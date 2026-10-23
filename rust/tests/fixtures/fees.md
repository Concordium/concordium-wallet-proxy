# Transfer and staking fee fixture provenance

Status: source-derived; independent review remains outstanding. These fixtures
were not recorded from the Rust estimator or from a running Haskell comparison
service. They cover Task 11, not contract-update or token-update estimation.

## Sources

- `src/Proxy.hs`: `getTransactionCostR`, `getMemoPayloadSize`, and `fetchUpdates`
  define aliases, parameter presence, validation order, protocol gating, and
  response fields. `src/Internationalization/En.hs` defines error wording.
- `deps/concordium-client/src/Concordium/Client/Types/Transaction.hs` defines
  payload sizes, normal/extended headers, and operation-specific costs.
- `deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Cost.hs`
  defines 100 energy per signature, one per byte, and operation costs.
- That base library's `Concordium/Types.hs` defines Word32 payload sizes,
  upward rational rounding in `computeCost`, and the 2048-byte default URL size.
  `Concordium/Types/Transactions.hs` defines the 60-byte header.
- `Concordium/Types/Execution.hs` defines validator keys with proofs as
  32 + 64 + 32 + 64 + 96 + 64 = 352 bytes.

## Expected calculations

Each JSON case uses a rate of 2/3 micro-CCD per energy. Cost is the decimal string
for `ceiling(energy * 2/3)`. Normal energy is
`100 * signatures + 60 + payload + operation cost`. Sponsorship adds 34 bytes
(two bitmap bytes and a 32-byte account address) only where the handler passes
extended cost options.

Payloads: simple transfer 41; encrypted transfer 2617; public-to-secret 9;
secret-to-public 1405; delegation registration 21 or 13 for passive; delegation
removal 11. Configure-delegation starts at 3 and adds 8 for amount, 1 for restake,
and 9 or 1 for an active or passive target. Configure-validator starts at 3 and
adds 8 for amount, 1 each for restake/open/suspension, 352 for keys, 2 plus metadata
size, and 4 per commission field. Registration includes all fields except
suspension and defaults metadata size to 2048.

Operation costs: simple transfer and validator-without-keys and delegation 300;
encrypted transfer 27000; public-to-secret 600; secret-to-public 14850;
validator-with-keys 4050. SDK constants are reused for these values, not SDK
transaction serialization or the u32-signature base-cost function.

## Coverage and limits

`fees.rs` checks all 13 aliases, all 512 configure-validator field combinations,
sponsorship presence (including empty, false, and junk values), first duplicate
query values, memo validation before protocol rejection, ignored parameters,
numeric syntax and wrap points, and exact prices beyond float precision. Integer
requests retain decimal/hex/octal syntax, outer parentheses, whitespace, signed
metadata, Word32 payload wraps, and 64-bit Int/energy wraps. Full equivalence to
every GHC `Read` lexical edge case has not received independent review.

`fees/rpc.rs` exercises the actual SDK and local gRPC transport for chain parameter
versions 0 through 3. It verifies consensus-before-parameters order, the Best
selector, both exchange-rate factors (1/10 times 500 = 50), no foundation-account
lookup, and failure short-circuiting before request validation. Malformed SDK
chain parameters currently become HTTP 502 INVALID_ARGUMENT rather than the
Haskell local-decoding error; this is the existing shared error-provenance
release blocker, not an approved difference. Node cookie forwarding is also
still unresolved. No public nodes or database changes are involved.

The SDK `EnergyRate::ccd_cost` uses remainder modulo `u64::MAX`; Haskell Amount
conversion uses Word64 (modulo 2^64). The estimator therefore performs exact
big-integer ceiling and Word64 conversion itself. Tests include both boundary
values and a product that exceeds u128.

Independent fixture review, observed CI execution, controlled real-node release
acceptance, and Task 01 acceptance remain required. Task 12 adds `update` and
`tokenUpdate`; see [extended fee provenance](fees-extended.md) for their evidence
and additional blockers. The replacement is not ready for production.
