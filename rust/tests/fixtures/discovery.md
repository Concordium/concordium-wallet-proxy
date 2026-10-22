# Public-key account discovery fixture provenance

Review status: source-derived; independent human review remains pending.

## Authority

`src/Proxy.hs`, route `KeyAccounts` and `getKeyAccounts`, defines the method,
path, key-before-filter validation, optional `onlySimple=y|n`, SQL equality,
ascending credential-index order, and five response fields. `lookupGetParam`
uses the first matching query parameter; there is no deduplication or tie-breaker.
`src/Internationalization/En.hs` prefixes parser errors with
`Parsing input failed: `. Database exceptions use the ordinary HTTP 500/error 0.

`Concordium.Crypto.Ed25519Signature`'s Serialize instance reads 32 bytes and
validates the compressed verification key. Its ToJSON instance uses lower-case
hex. `Concordium.Crypto.SignatureScheme`'s VerifyKey instance adds scheme byte 0
for stored keys, and renders JSON as `schemeId`/`verifyKey`. Thus the URL does
not include the scheme byte, while the BYTEA equality operand must include it.
Both modules are under `deps/concordium-client/deps/concordium-base/haskell-src/`.

`ByteStringSerialized`, `PersistField CredentialIndex`, and `PersistField KeyIndex`
in `src/Proxy.hs` establish raw BYTEA storage and Word8 conversion from INTEGER.
The `AccountPublicKeyBinding` declaration has no unique constraint or discovery
index. The test schema copies that subset, including BIGSERIAL primary key and
NOT NULL columns. Application code does not create, migrate, or write the table.

## Independently established expectations

`discovery.json` contains hand-written malformed-input responses. Source-reading
was supplemented by isolated GHCi library checks under this repository's Stack
environment (base16-bytestring 1.0.2.0 and cereal 0.5.8.3), not an HTTP comparison
service. The checks confirmed:

- Odd-length hex fails with `invalid bytestring size` before character validation.
- `0z` reports `invalid character at offset: 1`.
- Cereal's 32-byte read reports `too few bytes\nFrom:\tdemandInput\n\n`.
- Cereal accepts and ignores trailing decoded bytes.
- Thirty-two `02` bytes fail with `Failed reading: Cannot decode verification key.\nEmpty call stack\n`.
- A zero AccountAddress displays as `2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3`.
- Persistent's Word8 SQL type is SqlInt32, and PersistInt64 (-1) decodes to 255.

The successful HTTP expectations in `rust/tests/discovery.rs` explicitly select
all five fields, including the signature-scheme object. The uppercase-key check
uses the example public key from the main README's Key Accounts section. No
expectation is established solely by replacement output.

## Coverage and limits

Three socket-free HTTP tests cover success, empty results, controlled database
failure, parsing errors, precedence, repeated parameters, mixed-case hex, and
trailing bytes. Two real PostgreSQL tests use isolated synthetic databases to
check the tagged key encoding, filters, credential ordering, duplicate retention,
boundary indices, legacy Word8 wrapping, trailing address-byte acceptance,
invalid stored addresses, and missing-table failures. Equal credential indices
have no specified tie order; the duplicate fixtures are deliberately identical.

All tests are part of the existing integration target and CI's PostgreSQL and
metadata validation steps. They require no node and perform no production effects.
Independent fixture review and Task 01 acceptance remain outstanding.
