# 03: Serve wallet update guidance and terms

- **Type:** AFK
- **PRD coverage:** UC 20, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Provide update guidance for both existing wallet generations and supported platforms, together with the existing terms-and-conditions capability. Deliver configuration through HTTP responses with complete behavioral tests.

## Acceptance criteria

- [ ] Forced and suggested ranges, precedence, boundary values, platform selection, and destination links match current behavior.
- [ ] Missing configuration and malformed requests preserve current defaults, disabled behavior, statuses, and error messages.
- [ ] Invalid configured ranges or required links fail validation before serving.
- [ ] Independently reviewed fixtures and tests cover every existing version, platform, range boundary, and terms configuration combination relevant to behavior.
- [ ] Tests run in CI without comparing against a running Haskell implementation.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
