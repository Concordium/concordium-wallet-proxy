# 12: Estimate smart-contract and protocol-level token transaction fees

- **Type:** AFK
- **PRD coverage:** UC 4, 23; contract and token estimates

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Complete fee estimation for smart-contract updates and protocol-level token operations, including invocation results, energy buffers, encoded-operation sizes, and operation counts.

## Acceptance criteria

- [ ] Contract estimates preserve sender, contract, receive-name, parameter, amount, buffer defaults, rounding, and conditional success indicators.
- [ ] Token estimates support every currently accepted operation count and preserve size/count interpretation and costs.
- [ ] Missing and malformed input, execution rejection, protocol differences, and dependency failures match current validation and responses.
- [ ] Reviewed fixtures and controlled node-operation tests assert calculations and actual invocation inputs.
- [ ] Tests run in CI; unsupported SDK operations block completion instead of reducing supported modes.

## Implementation status

Implemented `update` and `tokenUpdate` in the existing v0 transaction-cost route.
Contract validation preserves sender/checksum errors, required-field order, full
receive names, raw parameter bytes, Word64 values, and signed buffer inputs. The
SDK invokes at Best with 500000 energy. Only execution energy receives the
default 20% buffer; multiplication, division, and addition retain legacy wrap
points. Success and rejection both return estimates with their success flag;
RPC failures remain errors without partial estimates.

Token estimates retain signed encoded sizes, UTF-8 identifier sizes, all 12
operation names, bounded Int JSON counts (including negative and integral
scientific values), first duplicate JSON keys, unknown-operation validation,
sponsorship, and Word32/Word64 conversion points. Syntax diagnostics use a
source-derived stack scanner rather than Serde message matching. Tokens are not
invoked or submitted. No SDK or Haskell source was changed. Provenance and
source-derived expectations are in `rust/tests/fixtures/fees-extended.md`.

**Acceptance blocker:** `Proxy.hs` references `tokenAssignRevokeRolesCost` and
`tokenUpdateTokenMetadataCost`, absent from its pinned Haskell base. Rust uses
SDK constants of 50 provisionally for assign/revoke roles and metadata updates.
Confirm the deployed Haskell source and costs or obtain an explicit decision;
this is not verified compatibility and no exception has been approved.

Seventeen new integration tests and one unit test cover exact invocation inputs,
execution success/rejection, buffers, payload and numeric boundaries, validation,
JSON diagnostics, every token operation, SDK transport, malformed node results,
query order, and invocation timeout. A local real-HTTP test exercises the SDK
invocation path. Validation: 136 tests pass serially (four unit and 132
integration); six existing PostgreSQL tests remain ignored. Locked offline
Clippy with warnings denied, formatting, and whitespace checks pass. Tests use
`TMPDIR=/tmp` because the environment's default temporary directory is absent.
The SDK retains its two existing lifetime-syntax warnings.

Not yet accepted: cost-source confirmation, independent fixture and complete
lexer/diagnostic review, Task 11/Task 01 acceptance, observed CI, and controlled
real-node release acceptance. Source/backend JSON-decoder mismatches and the
shared node-cookie and SDK error-provenance limits remain blockers, not approved
response differences. Acceptance checkboxes remain open.

## Blocked by

- [11: Transfer and staking fee estimation](11-transfer-and-staking-fee-estimation.md)
