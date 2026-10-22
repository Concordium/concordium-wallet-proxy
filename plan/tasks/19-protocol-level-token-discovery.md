# 19: Discover protocol-level tokens and retrieve token information

- **Type:** AFK
- **PRD coverage:** UC 17, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver the existing protocol-level token list and individual token-information capabilities using the SDK and explicit compatibility representations.

## Acceptance criteria

- [ ] Token identifiers, decimals, supply, module references, module state, and metadata references match current behavior.
- [ ] Empty lists, malformed or unknown identifiers, applicable protocol differences, and node failures preserve the current contract.
- [ ] Reviewed fixtures cover representative token configurations and optional state, boundary quantities, and failure responses.
- [ ] Controlled adapter tests verify relevant node operations and response conversion; CI requires deterministic tests.
- [ ] SDK serialization is used only when tests establish compatibility; missing support blocks completion.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
