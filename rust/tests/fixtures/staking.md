# Staking and chain-condition fixture provenance

Review status: source-derived expectations; independent human review pending.
Expected HTTP values are not recorded from replacement output. No test runs the
Haskell service as a comparison server or calls a public node.

## Sources

- `src/Proxy.hs`: `getBakerPoolR`, `getPassiveDelegationR`,
  `getChainParametersR`, `getNextPaydayR`, `getRewardPeriodLength`, and
  `firstPaydayAfter` define routes, query order, selectors, and custom failures.
- `deps/concordium-client/src/Concordium/Client/GRPC2.hs`:
  `getBlockChainParameters` resolves the foundation account index with an
  account-info query using the original block selector. Its pool conversion
  preserves the optional suspension flags and missed-round count.
- `deps/concordium-client/deps/concordium-base/haskell-src/Concordium/Types/Queries.hs`:
  pool-status JSON flattens active status, includes `poolType: BakerPool`, and
  emits `currentPaydayStatus: null` when absent. Optional suspension and missed
  rounds fields are omitted, not null. `PassiveDelegationStatus` is a single
  constructor with ordinary fields; its derived JSON has no `poolType` tag.
- The same base tree's `Concordium/Types/Parameters.hs`: the ToJSON instances for
  ChainParameters versions 0–3, reward parameters, mint distributions, gas
  rewards, commission ranges, and ratios define the response projection.
- `Concordium/Common/Time.hs` stores Duration as Word64 milliseconds;
  `firstPaydayAfter` multiplies the epoch count in that type before rounding.
- `src/Internationalization/En.hs` and the error wrappers in `src/Proxy.hs`
  establish exact English messages and error classifications.

## Fixture construction

`staking.json` contains a hand-written pool response, passive response, CPV0
response, and fields added by delegation and BFT parameter versions. The tests
assemble CPV1 by removing legacy cooldown/threshold/mint-per-slot fields and
adding delegation fields. CPV2 also removes election difficulty and gas
finalization-proof rewards and adds BFT fields. CPV3 adds maximum missed rounds.
The SDK's newer maximum lock duration and update keys are not part of the
Haskell response and are not leaked into HTTP.

Amounts are micro-CCD strings. Indices, counts, epochs, energies, and durations
are numeric. Cooldowns are seconds; consensus timeout/min-block durations are
milliseconds. Exchange rates, leverage, and timeout factors are explicit
numerator/denominator objects. Fractions and mantissa/exponent mint rates are
exact JSON decimals, including mint exponents beyond the SDK Serde decimal
range. Election difficulty's trailing decimal zeros have no numeric significance.

The controlled protobuf fixtures supply all legacy required chain fields,
including update-key access structures. The foundation account shares the
synthetic balance account's address and index 42. Pool and passive records use
known commissions, optional suspension values, null payday status, zero amounts,
large integers, and a maximum-u64 missed-round count.

Isolated Stack/GHCi type checks, not service comparisons, confirmed:

- `[1; 32]` as an AccountAddress renders as
  `2xBpaHottqhwFZURMZW4uZduQvpxNDSy46iXMYs9kceNGaPpZX`.
- Aeson encodes `Ratio Word64` as a numerator/denominator object.
- Word64 `fromPathPiece` accepts `+1` and leading zeros, but rejects negative,
  overflow, and trailing-junk values. Invalid pool paths therefore have the
  generic route 404/error 1, unlike missing or inactive pools (404/error 2).

## Query and boundary assertions

Pool queries use LastFinal. No active pool returns 404 before any parameter
queries. No pending change performs no follow-up queries. A pending change
reads chain parameters and the foundation account at the pool response hash.
Without reward-period parameters no estimate is added. Otherwise tokenomics is
queried at that same hash, followed by live consensus, and the effective time is
rounded up to the first payday at or after it. Fixtures cover reduction/removal,
exact payday and one-millisecond boundaries, legacy parameters, inconsistent
reward variants, and zero epoch duration. A unit test checks Word64 product wrap.

Chain-parameter requests query LastFinal twice: once for parameters and again
for the foundation account, not a hash substituted from the first response.
Passive delegation and next payday also use LastFinal. RewardStatusV0 and local
invariant failures on next payday map to 400/error 1; remote statuses retain
ordinary RPC mappings. Only the initial pool query customizes NOT_FOUND.

Seven controlled-adapter HTTP tests verify projections, selections, failure
mapping, and early termination. Three local SDK/gRPC tests verify outbound
requests and foundation lookup, including one real HTTP listener test for the
pending estimate and missed rounds. Three unit tests check exact mint boundaries,
required-field failures, and shared payday arithmetic. Existing CI includes all
of these tests without node or wall-clock dependencies.

## Acceptance limits

Independent fixture review and Task 01 acceptance remain pending. The shared
node-cookie and SDK error-provenance blockers still apply, including malformed
chain responses. Chrono's finite date range also limits computed estimates:
synthetic parameters whose estimated payday lies outside that range return an
internal error, whereas Haskell UTCTime can represent larger years. That
inherited timestamp-range limitation is not waived by this implementation.
