# 23: Validate deployment replacement and rollback

- **Type:** HITL
- **PRD coverage:** UC 27; operational safety NFRs

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Provide independently runnable deployment packaging and an operator-reviewed cutover/rollback procedure. Rehearse replacing and restoring the service against controlled existing-format state without a new gateway, runtime service, history producer, or data conversion. Production actions require explicit operator authorization.

## Acceptance criteria

- [ ] Replacement packaging supports the agreed startup configuration, secret handling, existing dependencies, and graceful shutdown without preserving old script/CLI names as a requirement.
- [ ] Controlled cutover and rollback preserve existing wallet integrations, transaction history, and persisted faucet state in both directions.
- [ ] The procedure prevents both implementations actively serving faucet requests for the same funding account and prohibits production shadowing of state-changing operations.
- [ ] Existing public health behavior supports verification; structured stderr diagnostics require no telemetry service or new public endpoints.
- [ ] Operator instructions include prerequisites, dependency/configuration mapping, traffic switch, health verification, rollback triggers, rollback steps, and ownership of faucet exclusivity.
- [ ] A human operator reviews rehearsal evidence and authorizes any real deployment separately; incomplete implementations receive no production traffic.
- [ ] No performance acceptance gate or database migration is introduced.

## Blocked by

- [22: Release compatibility review](22-release-compatibility-review.md)
