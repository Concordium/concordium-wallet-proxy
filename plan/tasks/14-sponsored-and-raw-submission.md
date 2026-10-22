# 14: Submit sponsored and raw transactions

- **Type:** AFK
- **PRD coverage:** UC 7–8, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete existing extended/sponsored transaction submission and bare binary transaction submission. Preserve protocol-level sponsorship, sender/sponsor authorizations, transaction contents, and existing binary acceptance rules.

## Acceptance criteria

- [ ] Valid sponsored submissions preserve sender and sponsor signatures and return compatible identifiers.
- [ ] Raw binary fixtures cover supported transaction kinds and representative malformed, incomplete, and unsupported encodings.
- [ ] Validation order, node rejection, dependency errors, and HTTP metadata match current behavior.
- [ ] Controlled adapter assertions verify submitted bytes or equivalent complete transaction content and prevent production effects.
- [ ] Reviewed fixtures and serialization tests run in CI; SDK gaps block completion without a silent fork or bypass.

## Blocked by

- [13: Credential and account submission](13-credential-and-account-submission.md)
