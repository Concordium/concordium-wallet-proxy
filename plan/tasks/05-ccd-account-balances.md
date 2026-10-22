# 05: Retrieve legacy and current CCD account balances

- **Type:** AFK
- **PRD coverage:** UC 1, 23; CCD balance portions

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver the existing CCD balance versions end to end, including current versus finalized account state, staking, delegation, cooldowns, release schedules, encrypted amounts, and available balance. Use SDK-backed queries and explicit compatibility conversions.

## Acceptance criteria

- [ ] Existing CCD balance versions preserve fields, omissions, numeric representations, and protocol-dependent staking information.
- [ ] Missing accounts and accounts created but not finalized preserve current version-specific behavior.
- [ ] Available balance, release schedules, cooldown phases, validator suspension, and encrypted balances match reviewed expectations.
- [ ] Independently reviewed fixtures cover each version and important boundaries; controlled node tests verify relevant block selection and failure handling.
- [ ] Tests run in CI; SDK gaps and behavior exceptions block completion pending explicit decision.

## Implementation investigation

Initially blocked before implementation by SDK 10.0.0 pool-status support. The existing
Haskell balance handler includes `isPrimedForSuspension` for validators and
pool delegators when the current-payday status supplies it, and `isSuspended`
for pool delegators when the active-pool status supplies it. The SDK generated
protobuf types contain these optional fields, but its public
`CurrentPaydayBakerPoolStatus` and `ActiveBakerPoolStatus` types and conversions
discard them. Account-level validator `is_suspended` does not replace the
missing pool-delegator fields.

Per PRD NFR 10, silently omitting the fields, guessing their values, privately
forking the dependency, or bypassing the agreed SDK boundary is not an approved
solution. A supported upstream SDK revision exposing these fields is preferred;
an alternative requires an explicit compatibility/boundary decision. No balance
routes have been added yet.

The user approved an upstreamable SDK fix. The `concordium-rust-sdk/` submodule
now has a local patch exposing both optional fields and preserving them in the
protobuf conversions. Four offline regression tests pass on Rust 1.85, covering
absent/false/true values, independent active/payday status, removed pools, and
JSON omission/round trips. Clippy passes for SDK library and test targets on
Rust 1.85. The patch is not yet committed/upstreamed; the proxy still depends on
crates.io SDK 10.0.0 and must be wired to the approved SDK before this slice can
use the fields. The full SDK library suite has five public-node-dependent tests
that fail on DNS resolution; the no-default-features build has unrelated genesis
serde errors. Neither limitation is claimed resolved by this patch.

Source observations to retain during implementation:

- This slice implements v0 and v1; token-inclusive v2 belongs to Task 06.
- v0 queries finalized state first, then best state; v1 only queries finalized.
- A missing finalized account returns `{}` without querying best state, even
  when the account could exist in an unfinalized block. The handler comment is
  not the implementation oracle for this quirk.
- Release, encrypted-amount aggregation, cooldown, and staking representations
  need explicit conversions rather than whole-account SDK serialization.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
