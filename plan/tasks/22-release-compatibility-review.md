# 22: Review complete compatibility and release evidence

- **Type:** HITL
- **PRD coverage:** UC 26; all compatibility and test NFRs

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Assemble and independently review the complete behavior coverage catalogue, fixture provenance, automated quality results, and controlled real-node end-to-end acceptance evidence. This is a release decision, not a deferred implementation/test phase. Include actual HTTP server behavior beyond isolated handler tests.

## Acceptance criteria

- [ ] Every existing route/version maps to passing success, important failure, and boundary scenarios; all PRD use cases and applicable NFRs have evidence.
- [ ] Review covers status codes, messages, method/path/query semantics, redirects, relevant headers, CORS, language precedence, omission/null, numeric precision, timestamps, ordering, binary inputs, and side effects.
- [ ] Fixture expectations are independently reviewed against current source behavior and documented examples; no Haskell comparison runner or replacement-output oracle is used.
- [ ] Required CI checks and controlled real-node acceptance pass; state-changing tests are isolated from production and public nodes are not required per PR.
- [ ] Database interoperability, faucet binary rollback compatibility, newer event support, concurrency, and dependency-failure policies have explicit evidence.
- [ ] SDK gaps, unexplained differences, and missing capabilities block approval unless specifically excepted by the user; no performance or line-coverage-percentage gate is added.
- [ ] A human release maintainer records approval or actionable blockers before deployment validation proceeds.

## Blocked by

- [02: Identity-provider catalogues](02-identity-provider-catalogues.md)
- [03: Wallet updates and terms](03-wallet-updates-and-terms.md)
- [04: Cryptographic and network information](04-cryptographic-and-network-information.md)
- [05: CCD account balances](05-ccd-account-balances.md)
- [06: Account token balances](06-account-token-balances.md)
- [07: Nonces and encryption keys](07-nonces-and-encryption-keys.md)
- [08: Public-key account discovery](08-public-key-account-discovery.md)
- [09: Staking and chain conditions](09-staking-and-chain-conditions.md)
- [10: Blocks and transaction outcomes](10-blocks-and-transaction-outcomes.md)
- [11: Transfer and staking fee estimation](11-transfer-and-staking-fee-estimation.md)
- [12: Contract and token fee estimation](12-contract-and-token-fee-estimation.md)
- [13: Credential and account submission](13-credential-and-account-submission.md)
- [14: Sponsored and raw submission](14-sponsored-and-raw-submission.md)
- [15: Submission progress](15-submission-progress.md)
- [16: CCD transaction history](16-ccd-transaction-history.md)
- [17: Token and lock history](17-token-and-lock-history.md)
- [18: Contract tokens](18-contract-tokens.md)
- [19: Protocol-level token discovery](19-protocol-level-token-discovery.md)
- [20: Testnet CCD drops](20-testnet-ccd-drops.md)
- [21: Transak purchase sessions](21-transak-purchase-sessions.md)
