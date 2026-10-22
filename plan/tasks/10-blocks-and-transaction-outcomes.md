# 10: Inspect blocks and block transaction outcomes

- **Type:** AFK
- **PRD coverage:** UC 22, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Expose existing block information, blocks-at-height, and block transaction-outcome queries using the SDK and project-owned compatibility conversion. Resolve event support required by these capabilities within this slice.

## Acceptance criteria

- [ ] Block selection, heights, hashes, timestamps, outcomes, ordering, and conditional fields match current behavior.
- [ ] Fixtures exercise supported historical and current event variants, including sponsorship and lock-related events where applicable.
- [ ] Invalid references, missing blocks, empty results, and node failures preserve existing responses.
- [ ] Independently reviewed HTTP fixtures and controlled SDK adapter tests run in CI.
- [ ] Unsupported SDK variants block completion; no silent omission, fork, or protobuf bypass is introduced.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
