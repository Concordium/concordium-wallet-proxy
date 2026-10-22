# 11: Estimate fees for transfers, staking, and delegation

- **Type:** AFK
- **PRD coverage:** UC 4, 23; non-contract and non-token estimates

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver all current transfer, encrypted-transfer, staking, and delegation fee-estimation modes, including configure-validator options and existing aliases. Use SDK/base primitives where compatible and preserve current wallet-facing calculations.

## Acceptance criteria

- [ ] Energy and CCD costs preserve signature counts, memo sizes, metadata sizes, commission options, suspension options, defaults, and rounding.
- [ ] Existing transaction aliases yield compatible results and parameter-presence semantics are preserved.
- [ ] Missing or malformed parameters, numeric boundaries, protocol restrictions, and node failures match validation order and errors.
- [ ] Reviewed fixtures and calculation/adapter tests cover all supported modes and relevant combinations.
- [ ] CI runs deterministic tests without a public node; unapproved calculation changes are not introduced.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
