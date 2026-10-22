# 15: Report submission progress and transaction outcomes

- **Type:** AFK
- **PRD coverage:** UC 9, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver submission-status lookup for transaction hashes and existing submission identifiers, with compatibility conversion for conditional outcome information across the transaction lifecycle.

## Acceptance criteria

- [ ] Absent, received, committed, and finalized states retain existing response behavior.
- [ ] Success, rejection, and ambiguous committed outcomes preserve block references, descriptions, and conditional transaction details.
- [ ] Covered details include transfer/encrypted effects, credential deployment, contract initialization/update traces, and other currently represented variants.
- [ ] Malformed identifiers, inconsistent node responses, and transport failures match existing error behavior.
- [ ] Reviewed fixtures and controlled node adapter tests run in CI; omitted-versus-null distinctions and numeric formats are asserted.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
