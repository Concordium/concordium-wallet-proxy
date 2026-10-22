# CCD balance compatibility expectations

These are source-derived expectations, not captures of replacement output.
Independent human review remains outstanding.

## Sources

- `src/Proxy.hs:getAccountBalanceR`, including `getAccountAndPoolInfo`,
  `doGetBal`, and `onResponse`: field selection, version differences, pool
  queries, failure suppression, and query order.
- `src/Proxy.hs:pendingChangeToJSON` and `firstPaydayAfter`: pending change
  fields and rounding up to the first payday at or after the cooldown end.
- `Concordium/Types.hs:ToJSON AccountEncryptedAmount` in the Haskell base
  submodule: aggregated ciphertext precedes incoming ciphertexts; its count is
  optional. SDK whole-encrypted-balance JSON has a different structure and is
  not used as the HTTP expectation.
- `Concordium/Types/Accounts.hs:ToJSON Cooldown` and `BakerPoolInfo`:
  cooldown timestamps are integer milliseconds and statuses are `cooldown`,
  `precooldown`, and `preprecooldown`; pool metadata and commission field names.
- `Concordium/Types/Execution.hs:ToJSON DelegationTarget` and
  `Concordium/Common/Time.hs:Timestamp`: delegation target tags and timestamp
  units. Staking pending-change times instead use Aeson's UTC time strings.
- `src/Internationalization/En.hs` and `runGRPCWithCustomError`: malformed
  address messages and node error status/message mappings.

## Inputs and coverage

`current.json` describes the inner v1 balance for the controlled protobuf account
in `tests/balances/fixtures.rs`. The v0 expectation removes only cooldowns and
available balance, then includes both finalized and current state. The best
account amount is deliberately different in the version-selection test.
The base fixture uses amounts, a nonce, and an encrypted index beyond JavaScript's
exact integer range. Releases include multiple hashes and retain their order.

Additional source-derived expectations in the tests cover validators, passive
and pool delegation, every optional suspension-flag value, removed pools,
legacy validator metadata omission, zero/max-u64 balances, account-not-found
short circuits, and failures at each query stage. Available balance tests cover
node-supplied values and the SDK fallback formula:

`accountAmount - max(releaseTotal, activeStake + inactiveStake)`.

Ciphertext fixtures use the standard compressed identity and generator points;
expected hex strings are constructed from input bytes, not the JSON serializer
under test. Test validator keys are deterministically generated with seed 42;
expected key strings use their binary encoding rather than SDK JSON output.
No production keys are used.

Pending-change cases include before, exactly at, and just after payday
boundaries; missing payday/reward-period information; and simultaneous pending
changes in finalized and best state with a single chain-parameter query.
The estimate uses finalized-block timing inputs for both balance versions.

The controlled gRPC tests assert account identifiers, validator IDs, actual
outbound block selectors, query order, pool suspension fields, tokenomics
variants, and conditional chain-parameter lookups through the real SDK. A local
HTTP server test checks concurrent v0/v1 requests through SDK transport. These
tests need no public node or database and do not submit transactions.

## Known limits

SDK response-conversion failures can still be indistinguishable from remote RPC
statuses. Node-cookie propagation is still unsupported. These are documented
compatibility/release blockers, not approved behavior exceptions. The minimal
protobuf fixtures omit unused credential contents; this slice does not establish
credential-format compatibility. V2 token enrichment is intentionally separate
(Task 06), not a partially implemented route.
