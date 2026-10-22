# 09: Expose staking pools, delegation, and chain conditions

- **Type:** AFK
- **PRD coverage:** UC 14, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver validator-pool status, passive delegation status, chain parameters, and next-payday queries through SDK-backed operations with explicit wallet-facing representations.

## Acceptance criteria

- [ ] Existing protocol-dependent values, optional properties, time calculations, and numeric representations remain compatible.
- [ ] Unknown pools, invalid path values, unsupported protocol conditions, and node errors match current behavior.
- [ ] Reviewed request/response fixtures cover each capability and important protocol transitions and boundaries.
- [ ] Controlled adapter tests verify node query selection and error mapping; time-dependent tests use controlled time where necessary.
- [ ] Tests run in CI; dependency gaps require an explicit decision rather than omitted fields or endpoints.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
