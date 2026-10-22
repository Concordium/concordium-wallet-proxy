# 21: Create Transak purchase sessions

- **Type:** AFK
- **PRD coverage:** UC 21, 23

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver optional Transak purchase-session creation with configured production/staging selection, credentials, referrer, user IP source, allowed browser origins, and compatible token-refresh coordination.

## Acceptance criteria

- [ ] Valid requests preserve outbound session contents, returned purchase destination, and existing request validation.
- [ ] Expired tokens, concurrent refresh requests, refresh/session failures, timeouts, and disabled configuration match current behavior.
- [ ] Browser-origin and preflight tests cover allowed/disallowed/missing origins and retain the distinction between Transak and public CORS policies.
- [ ] Controlled HTTP service tests assert authentication/session operations, user IP selection, and relevant headers; time-dependent behavior is deterministic.
- [ ] Independently reviewed HTTP fixtures and concurrency tests run in CI without real purchase sessions or production credentials.

## Blocked by

- [01: Startup and health](01-startup-and-health.md)
