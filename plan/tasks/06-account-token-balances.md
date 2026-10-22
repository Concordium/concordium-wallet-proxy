# 06: Retrieve account balances including protocol-level tokens

- **Type:** AFK
- **PRD coverage:** UC 1, 23; token-inclusive balance portions

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete the token-inclusive account balance version using SDK-backed account and token queries. Preserve CCD information and enrich protocol-level token holdings with the existing token and account module-state representation.

## Acceptance criteria

- [ ] Token holdings preserve identifiers, raw amounts, decimals, module references, supply, metadata references, and applicable allow-list/deny-list state.
- [ ] Empty holdings, absent module-state properties, missing accounts, and protocol-dependent behavior match current implementation semantics.
- [ ] Node-operation sequencing and partial query failures produce compatible responses rather than silently dropping information.
- [ ] Reviewed HTTP fixtures and controlled adapter tests cover multiple tokens, boundary amounts, and dependency failures.
- [ ] Tests run in CI and do not implicitly adopt SDK serialization as the HTTP contract.

## Blocked by

- [05: CCD account balances](05-ccd-account-balances.md)
