# 17: Browse protocol-level token and lock-related history

- **Type:** AFK
- **PRD coverage:** UC 10–11, 23; token-inclusive history

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete the token-inclusive history version, retaining its CCD behavior and adding existing protocol-level token creation, governance, holder, and lock-related summary/event representations. Resolve legacy summary and SDK support within this slice.

## Acceptance criteria

- [ ] Token and CCD selection, version boundaries, filters, pagination, and response details match current behavior.
- [ ] Supported governance operations, sponsored effects, lock creation/destruction, and token transfers with lock metadata decode and render compatibly where currently exposed.
- [ ] Real PostgreSQL fixtures cover historical/current summary forms, token rejections, mixed activity, and applicable optional event metadata.
- [ ] Reviewed HTTP fixtures and checked SQLx metadata are validated in CI alongside legacy-history regression tests.
- [ ] Unsupported formats or dependency variants remain explicit blockers; no event or transaction is silently discarded.

## Blocked by

- [16: CCD transaction history](16-ccd-transaction-history.md)
