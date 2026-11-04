# Testnet CCD drop compatibility evidence

## Source and fixture provenance

`src/Proxy.hs` (`GTURecipient`, `DropSchedule`, `putGTUDropR`, and
`doSendBlockItem`) is the behavioral authority. `app/Main.hs` supplies
`accountParser`, `--drop-account`, and the 2000 CCD default. The table name
`g_t_u_recipient` and `unique_account` follow Persistent's generated names
(also present in the locally built `Proxy.o`); the account and transaction
columns use `ByteStringSerialized`, not JSON or a tagged block item.

`faucet/generate.py` independently builds both binary vectors using Python
`struct`, SHA-256, and cryptography's Ed25519 implementation. It does not import
the Rust SDK or run the Haskell service. The synthetic signing key is the
published RFC 8032 test key, never a funding key. These are source-derived
legacy-format fixtures, **not captures of Haskell-produced database rows**.
Independent human review and actual historical-state/rollback evidence are
still required before acceptance.

Serialization authorities in
`deps/concordium-client/deps/concordium-base/haskell-src/`:

- `Concordium/Types/Transactions.hs`: account transaction serialization is
  signature map, header, encoded payload. Header is sender, nonce, energy,
  payload length, expiry. The sign hash is SHA-256(header + payload); the
  submission hash includes the normal block-item tag `0` before transaction bytes.
- `Concordium/Crypto/SignatureScheme.hs`: each signature has a u16 length prefix.
- `Concordium/Types/Execution.hs`: transfer and scheduled payload encodings.
- `Concordium/Cost.hs` and client `Types/Transaction.hs`: normal energy is
  `60 + 41 + 100 + 300 = 501`. For n releases with one signature,
  scheduled energy is `60 + (34 + 16n) + 100 + 364n = 194 + 380n`.

Both fixtures use sender `07` repeated 32 times, recipient `08` repeated 32
times, nonce 10, POSIX time 1000, expiry 1300, and amount 2,000,000,001 microCCD.
The scheduled fixture has four releases: the first at 1,600,000 milliseconds
contains 500,000,001 microCCD; the remaining three are spaced by 300,000
milliseconds and contain 500,000,000 microCCD each.

| Fixture | Bare length | Submission SHA-256 (tag + bare transaction) |
| --- | --- | --- |
| `faucet/normal.bin` | 171 | `5a701cf3066d20887bc359f8422b0822b653c0ed44572e22be5db03f37057634` |
| `faucet/scheduled.bin` | 228 | `8c111f75129a2e9aff8eb9f8f184eb415584f1ce40078d806a852103209b8c16` |

## Preserved behavior

- Disabled requests return HTTP 404/code 2 before address/body validation.
- Finalized recipient existence is checked before body interpretation or storage.
  A missing finalized recipient returns HTTP 400/code 1, not the comment's 404.
- Empty or malformed JSON selects normal transfer. Valid JSON with an invalid
  schedule returns HTTP 400 configuration error. JSON parsing retains the first
  duplicate key and accepts a complete value before trailing input, using the
  shared submission-compatible parser.
- Key selection uses ascending credentials and keys up to their thresholds.
  Energy counts actual selected keys, including undersupplied key maps, as in Haskell.
- Time rounds POSIX seconds to nearest, ties to even. Expiry is now + 300 seconds;
  schedules sample 1–15 releases. The legacy first release is **now + 600 seconds**,
  not now + 300; any remainder goes into the first release.
- A newly signed bare transaction is committed before node submission. Unique
  recipient conflicts discard the new candidate and restart the lookup.
- Existing transactions query the **recipient's** finalized nonce/balance, despite
  source comments describing the funding account. Received, committed, and
  successfully finalized transactions return the stored hash without resubmission.
- Absent or rejected transactions check recipient balance before nonce/expiry.
  The amount + 1,000,000 microCCD threshold retains unsigned wrapping arithmetic.
  Replacement occurs only when finalized nonce is greater than stored nonce or
  stored expiry is strictly less than rounded current time. Equality is reusable.
- Failed submissions retain persisted bytes and return HTTP 400/code 1
  `Server configuration error`; ordinary query failures use the shared node policy.

## Tests and operational boundary

`rust/tests/faucet.rs` and child modules check request precedence, controlled
signatures, explicit energy/header/payload/schedule values, fixed binary fixtures,
repeated requests, conflict retries, corrupt stored bytes, insufficient funds,
nonce invalidation, expiry equality, and node/storage failures.
`progress/rpc.rs` checks the production faucet status adapter with received,
committed, successful/rejected finalized, missing, and malformed responses.
Existing account/submission SDK transport tests remain regression coverage for
the delegated nonce/account/submission adapters.

Three ignored-by-default PostgreSQL tests run against isolated databases. They
assert unique recipients, row-specific deletion, exact byte reuse, independent
visibility of committed bytes before submission, no external table mutation,
HTTP database failures, and a forced two-request insert race with only the winning
transaction submitted. CI includes ignored database tests using its disposable
PostgreSQL service. Queries have checked offline metadata.

Only one implementation may actively serve a faucet funding account. See the
operator ownership and rollback instructions in `rust/README.md`. Tests perform
no public-node queries or real funding operations.

## Remaining acceptance

Independent schema/fixture/signature/parser review, actual Haskell-produced-state
and rollback evidence, observed CI, task dependency acceptance (07/13/15), shared
deployed-node/dependency alignment, and controlled end-to-end acceptance remain
open. Exotic time/amount overflow, malformed key-map acceptance, JSON depth limits,
and SDK response-conversion provenance require the same explicit compatibility
review as earlier tasks; do not infer full production compatibility from these
synthetic fixtures. No production rollout is authorized.
