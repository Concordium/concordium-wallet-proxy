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

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
