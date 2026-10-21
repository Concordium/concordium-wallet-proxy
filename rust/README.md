# Rust Wallet Proxy

The Cargo package is rooted at the project root. Rust sources live here so the
existing Haskell application remains independently buildable and unchanged.

## Status

The first slice implements startup, configuration, required cryptographic-parameter
acquisition, and `GET /v0/health`. It is **not a production replacement**. Other
wallet capabilities remain in the Haskell service. Task 01 is not signed off:
PostgreSQL validation, unrestricted transport tests, independent fixture review,
and the compatibility issues below remain outstanding.

## Known compatibility issues

### SDK request metadata and node cookies — unresolved, release-blocking

The Haskell node client sends a configured `target` metadata header, passes request
cookies to the node, and forwards the first node `set-cookie` response headers to
the wallet. SDK 10's high-level client has a private transport, no custom metadata
hook, and returns decoded responses without exposing those cookies.

The current Rust implementation cannot reproduce those behaviors. Configuration
with `--node-target` or `WALLET_PROXY_NODE_TARGET` is rejected explicitly instead of silently ignoring it. Direct
node connections can be used for development; cookie forwarding is not supported.
Continuing development with this issue documented does **not** waive full release
compatibility. No SDK fork, direct protobuf bypass, or additional production proxy
has been introduced. Prefer an upstream SDK extension and pinned supported
revision when resolving this issue.

Sources: Haskell `mkGrpcClient` and `runClientWithCookies` in the client dependency,
`runGRPCWithCustomError` in the proxy; SDK `v2::Client`, `Client::new`, and
`QueryResponse`.

### SDK error provenance — unresolved, release-blocking

The health handler distinguishes client/network failure from a non-OK node RPC
response and from a response-conversion invariant failure. SDK conversion errors
can become the same Tonic status type as remote RPC failures. The adapter handles
transport errors with a source, explicit parse errors, deadlines, and ordinary RPC
statuses, but cannot reliably distinguish every SDK conversion failure from a
remote status. In particular, some malformed responses may currently yield an
unhealthy response instead of the legacy internal-error response. This needs SDK
support or an explicitly approved resolution; a service-level error fixture is
not proof that the real SDK adapter preserves every failure distinction.

## Configuration and startup

Use Rust 1.94 or newer. Clap reads command-line flags and environment variables;
there is no application configuration file or automatic `.env` loading. Supply
an explicit node endpoint, the three real catalogue files, and exactly one database
URL source. The node endpoint has no default:

```sh
cargo fetch --locked
cargo run --locked -- \
  --database-url-file /run/secrets/database-url \
  --node-endpoint https://node.example:20000 \
  --ip-data /data/catalogs/v0.json \
  --ip-data-v1 /data/catalogs/v1.json \
  --ip-data-v2 /data/catalogs/v2.json
```

Every setting has a corresponding `WALLET_PROXY_` environment variable. For example:

```sh
export WALLET_PROXY_DATABASE_URL_FILE=/run/secrets/database-url
export WALLET_PROXY_IP_DATA=/data/catalogs/v0.json
export WALLET_PROXY_IP_DATA_V1=/data/catalogs/v1.json
export WALLET_PROXY_IP_DATA_V2=/data/catalogs/v2.json
export WALLET_PROXY_NODE_ENDPOINT=https://node.example:20000
export WALLET_PROXY_LISTEN=127.0.0.1:3001
cargo run --locked -- --listen 127.0.0.1:3002
```

Command-line flags override the same option's environment value; environment
values override defaults. The example listens on port 3002. Run
`cargo run --locked -- --help` for every flag, environment name, and default.
`rust/config.example.env` is a shell example that can be sourced explicitly.

An `https://` node endpoint enables TLS with system trust roots and verification
of the endpoint hostname. An `http://` endpoint disables TLS. Custom CA files and
TLS-domain overrides are not supported; the Haskell service has no such options.

Use exactly one of `--database-url` / `WALLET_PROXY_DATABASE_URL` or
`--database-url-file` / `WALLET_PROXY_DATABASE_URL_FILE`. These are different,
mutually exclusive options: setting one via a flag does not clear the other from
the environment. Prefer the file option for secrets; inline command-line URLs can
appear in process listings. Help output hides the database URL environment value.
Secret files have trailing line endings removed. Validation errors do not include
the database URL.

Set `--database-max-connections` or `WALLET_PROXY_DATABASE_MAX_CONNECTIONS` to
limit PostgreSQL pool connections. The default is 10; zero and negative values
are rejected. This setting is independent of the database URL source.

Relative paths start at the process working directory, for both flags and
environment values. `--enable-public-cors` enables public CORS;
`--enable-public-cors=false` can override a true environment value. Environment
booleans accept `true` or `false`. Unknown flags are rejected; unrelated environment
variables are ignored. The old `--config`, `WALLET_PROXY_CONFIG`, and double-underscore
environment convention are no longer supported.

Defaults preserve the existing listen address/port, 300-second health tolerance,
15-second RPC timeout, zero additional connection attempts, ten database pool
connections, disabled public CORS, and disabled logging. Startup validates
configuration and catalogue JSON, connects to PostgreSQL and the node, and acquires
last-final cryptographic parameters before binding the HTTP listener. No external
table is created or migrated. Configuration changes require restart. Interrupt
and termination signals initiate graceful HTTP shutdown.

Optional wallet features and their configuration will be added in their own
slices; unknown settings are not silently accepted as implemented capabilities.

## Health compatibility

The handler follows `getHealthR` in the Haskell proxy:

- A missing/non-OK node result returns HTTP 200 with `healthy: false` and skips SQL.
- A valid last-final block triggers a one-row Cartesian-product read of `ati` and
  `summaries`, including row decoding. Empty tables are allowed; this is not a join
  or a database freshness check.
- The block is healthy only when its age is strictly less than the configured
  tolerance. An exact boundary is stale; a future block is not newly rejected.
- Database exceptions use the generic internal-server-error contract, not the
  account-history-specific database error message.
- Time rendering trims trailing fractional zeros to match Aeson's UTC rendering.
- The reported version follows the package version, currently 0.50.0 from the
  Haskell package metadata. The ignored generated Cabal file is older.

Request/response fixtures record source provenance and are not captured from
replacement output. Independent human review is still required before sign-off.
English is currently the sole translation, so all supported preference sources
and fallback combinations select English for localized errors. Success health
responses do not gain an unrelated language header.

## Dependencies

`Cargo.lock` records resolved dependencies. The Concordium Rust SDK is pinned to
published 10.0.0; no sibling checkout is required. Tonic stays on the SDK-compatible
0.12 line so its transport types and TLS features match the SDK. The other direct
dependencies were updated to current stable releases when the project was created.

## Tests and query checking

```sh
cargo fmt --all -- --check
cargo clippy --locked --all-targets -- -D warnings
cargo test --locked --all-targets
```

The normal suite includes in-process HTTP fixtures and a local HTTP/2 gRPC test
server exercised through the actual SDK. It requires permission to bind loopback
listeners; socket failures are not silently skipped. No Haskell executable, public
node, production database, or real Transak session is used.

PostgreSQL tests are explicitly ignored by default because they create isolated
test databases. Run them only against disposable PostgreSQL with a role permitted
to create databases:

```sh
export DATABASE_URL=postgres://postgres:postgres@localhost:5432/wallet_proxy_test
cargo test --locked --all-targets -- --include-ignored
```

The SQL schema fixture is test-only and currently covers the legacy health tables
and relevant index. It is not a production migration. Integration tests cover
empty/populated tables, decoding/schema failures, actual SDK plus PostgreSQL plus
HTTP startup, and required startup-query failure.

Application queries use SQLx compile-time checking and committed offline metadata.
Normal builds use `SQLX_OFFLINE=true` through Cargo configuration. The initial
metadata cache is seeded from the test schema, **not yet regenerated or verified
against a live PostgreSQL instance in this development environment**. CI is
configured to validate it against real PostgreSQL; a passing offline compilation
alone does not establish database correctness.

To regenerate/check metadata, install SQLx CLI 0.9.0, prepare a disposable database,
and run from the project root:

```sh
cargo install sqlx-cli --version 0.9.0 --locked --no-default-features --features postgres,rustls
psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -f rust/tests/fixtures/schema.sql
SQLX_OFFLINE=false cargo sqlx prepare -- --all-targets
SQLX_OFFLINE=false cargo sqlx prepare --check -- --all-targets
```

Use a fresh database for the schema-loading command. CI runs formatting, Clippy,
all tests including PostgreSQL, and live metadata validation without a public
node. The workflow has been added but has not yet been observed running.
