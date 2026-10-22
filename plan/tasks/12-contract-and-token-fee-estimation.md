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

## Blocked by

- [11: Transfer and staking fee estimation](11-transfer-and-staking-fee-estimation.md)
