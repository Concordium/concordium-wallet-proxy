# 04: Expose cryptographic parameters and network information

- **Type:** AFK
- **PRD coverage:** UC 13, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Expose existing cryptographic parameters, genesis identity, epoch length, and consensus information through SDK-backed behavior and project-owned HTTP compatibility representations. Preserve startup caching of cryptographic parameters rather than adding new caching semantics.

## Acceptance criteria

- [ ] Every existing capability returns compatible fields, omissions, numeric representations, units, and timestamps.
- [ ] Startup-acquired cryptographic parameters retain their existing versioned representation and lifecycle.
- [ ] Controlled SDK adapter tests and independently reviewed HTTP fixtures cover success, relevant protocol variants, and node failures.
- [ ] SDK serialization is reused only where tests prove compatibility; unsupported variants are surfaced as blockers.
- [ ] Tests run in CI without public-node dependence or production writes.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
