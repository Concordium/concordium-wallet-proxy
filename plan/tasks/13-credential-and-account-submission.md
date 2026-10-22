# 13: Submit credential deployments and account transactions

- **Type:** AFK
- **PRD coverage:** UC 5–6, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver existing credential-deployment and standard account-transaction submission paths from accepted request formats through SDK node submission to compatible submission identifiers and errors.

## Acceptance criteria

- [ ] Supported credential versions and existing standard transaction request formats decode compatibly and preserve submitted content.
- [ ] Accepted submissions return the expected identifier and response; malformed requests, node rejection, and transport failures match current behavior.
- [ ] Reviewed request/response fixtures and deterministic serialization tests verify identifiers and input interpretation.
- [ ] Controlled adapters assert exact submitted operations and retry behavior, not only HTTP responses.
- [ ] Tests never submit into production and run in CI; no unapproved retry policy or unsupported-format omission is introduced.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
