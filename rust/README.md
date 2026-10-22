# Rust Wallet Proxy

The Wallet Proxy provides a wallet-facing HTTP API for Concordium. It connects to
a Concordium node and the existing transaction-history database, and serves
identity-provider information used by wallet issuance and recovery flows.

## Overview

The service exposes these endpoints:

- `GET /v0/health`: check node access, transaction-history database access, and
  the age of the last finalized block.
- `GET /v0/ip_info`: get the identity-provider catalogue for the initial-account
  issuance flow.
- `GET /v1/ip_info`: get the configured identity-provider catalogue for issuance
  and recovery without initial accounts.
- `GET /v2/ip_info`: get the configured identity-provider catalogue, including
  company identity flows.

Errors use the existing wallet-facing JSON format:

```json
{"error": 0, "errorMessage": "Internal server error"}
```

See the known compatibility issues below for differences from the Haskell service.

## Dependencies

### Runtime dependencies

- A Concordium node with an accessible gRPC V2 endpoint.
- PostgreSQL containing the existing transaction-history tables.
- Three identity-provider catalogue JSON files, one for each API version.
  See the [identity-provider metadata documentation](../README.md#identity-providers-metadata-files)
  for the usual file contents.

## Build and run

Run the following commands from the project root:

```sh
cargo build --release --locked
export WALLET_PROXY_DATABASE_URL='postgresql://user:password@database:5432/transaction-outcome'
./target/release/concordium-wallet-proxy \
  --node-endpoint https://node.example:20000 \
  --ip-data /data/catalogs/v0.json \
  --ip-data-v1 /data/catalogs/v1.json \
  --ip-data-v2 /data/catalogs/v2.json
```

Set `WALLET_PROXY_DATABASE_URL` through your deployment's environment or secret
management system. Avoid putting credentials in command-line arguments or shell
history.

For development, use `cargo run --locked --` with the same arguments. Show all
flags, environment variables, and defaults with:

```sh
cargo run --locked -- --help
```

The server listens on `0.0.0.0:3000` by default. Query it with:

```sh
curl http://localhost:3000/v0/health
curl http://localhost:3000/v2/ip_info
```

Startup validates settings and catalogue JSON, connects to PostgreSQL and the
node, and obtains cryptographic parameters before opening the HTTP listener.
Interrupt and termination signals initiate graceful HTTP shutdown.

## Configuration

Can be configured using command-line flags and environment variables. Flags override the same
option's environment value; environment values override defaults.

[`config.example.env`](config.example.env) provides a shell example that can be
sourced explicitly. Relative paths start at the process working directory.
Configuration and catalogue changes require a restart.

### Database settings

Supply the required PostgreSQL URL through `WALLET_PROXY_DATABASE_URL`.
Environment variables are recommended for credentials because command-line
arguments can appear in process listings. `--database-url` remains available as
an explicit override.

Set `--database-max-connections` or `WALLET_PROXY_DATABASE_MAX_CONNECTIONS` to
limit pool connections. The default is 10; zero and negative values are rejected.
The pool limit is independent of the database URL.

### Node and server settings

- `--node-endpoint`: required HTTP or HTTPS node address. Custom CA files and
  TLS-domain overrides are not supported.
- `--node-timeout-seconds`: RPC and connection-attempt timeout; defaults to 15.
- `--node-connect-retries`: additional connection attempts; defaults to zero.
- `--listen`: HTTP listen address; defaults to `0.0.0.0:3000`.
- `--health-tolerance-seconds`: block age limit; defaults to 300.
- `--log-filter`: log selection; defaults to `off`. Logs use structured JSON on
  standard error.
- `--enable-public-cors`: permit browser requests from any origin on public
  routes. CORS is disabled by default. Use `--enable-public-cors=false` to override
  a true environment value.

Environment booleans accept `true` or `false`. Unknown flags are rejected;
unrelated environment variables are ignored. Use `--help` for the corresponding
`WALLET_PROXY_` names.

## Configuration migration from Haskell

Replace the Haskell entrypoint with the Rust executable. Rename the environment
variables as shown below.
Use `--help` to check which flags Rust accepts.

| Haskell flag | Haskell Docker variable | Rust environment variable |
| --- | --- | --- |
| `--db` | `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_NAME` | `WALLET_PROXY_DATABASE_URL` |
| `--grpc-ip`, `--grpc-port`, `--secure` | `GRPC_HOST`, `GRPC_PORT`, `USE_TLS` | `WALLET_PROXY_NODE_ENDPOINT` |
| `--grpc-timeout` | `GRPC_TIMEOUT` | `WALLET_PROXY_NODE_TIMEOUT_SECONDS` |
| `--grpc-retry` | `GRPC_RETRY` | `WALLET_PROXY_NODE_CONNECT_RETRIES` |
| `--port` | — | `WALLET_PROXY_LISTEN` |
| `--health-tolerance` | — | `WALLET_PROXY_HEALTH_TOLERANCE_SECONDS` |
| `--ip-data` | `IP_DATA_FILE` | `WALLET_PROXY_IP_DATA` |
| `--ip-data-v1` | `IP_DATA_FILE_V1` | `WALLET_PROXY_IP_DATA_V1` |
| `--ip-data-v2` | `IP_DATA_FILE_V2` | `WALLET_PROXY_IP_DATA_V2` |
| `--forced-update-config-v0` | `FORCED_UPDATE_CONFIG_FILE_V0` | `WALLET_PROXY_FORCED_UPDATE_CONFIG_V0` |
| `--forced-update-config-v1` | `FORCED_UPDATE_CONFIG_FILE_V1` | `WALLET_PROXY_FORCED_UPDATE_CONFIG_V1` |
| `--tc-version` | `TC_VERSION` | `WALLET_PROXY_TC_VERSION` |
| `--tc-url` | `TC_URL` | `WALLET_PROXY_TC_URL` |
| `--log-level` | `LOG_LEVEL` | `WALLET_PROXY_LOG_FILTER` |
| `--enable-public-cors` | `ENABLE_PUBLIC_CORS` | `WALLET_PROXY_ENABLE_PUBLIC_CORS` |

### Wallet updates and terms

Supply optional update JSON files with `--forced-update-config-v0` and
`--forced-update-config-v1`. Each file can contain `ios` and `android` objects
with `forceUpdateVersions`, `suggestUpdateVersions`, `url`, and `suggestUrl`.
Version ranges use decimal integers from 0 through 18446744073709551615:
`5,8,10-17`, `-64`, or `64-`. Bounds are inclusive. `*` matches every
version; an empty string matches none. Commas combine ranges. Whitespace,
empty comma-separated entries, reversed bounds, overflow, and trailing junk
are rejected. A bare `-` is invalid.
Non-empty ranges require `url`; `suggestUrl` defaults to `url`.
Files are loaded before dependency connections and must be readable JSON.

Set both `--tc-version` and `--tc-url` to enable
`/v0/termsAndConditionsVersion`. With either field absent, this route returns 404.

### Convert values

1. Set the node endpoint explicitly, for example `https://node.example:20000`.
   Use `https://` for `USE_TLS=true`; otherwise, use `http://`. There is no default endpoint.
2. Set the listen address and port, for example `0.0.0.0:3000`, not just `3000`.
3. Change log level `warning` to `warn`. Set the filter explicitly.
   Rust defaults to `off`; the Haskell Docker entrypoint defaults to `debug`.
4. Keep time values in seconds and CORS values as `true` or `false`.
5. Reuse the catalogue files and database. No data conversion or schema migration is required.

### Convert the database URL

Convert the libpq connection string:

```text
host=db.example port=5432 user=wallet password=p@ssword dbname=transaction-outcome
```

Use this PostgreSQL URL:

```text
postgresql://wallet:p%40ssword@db.example:5432/transaction-outcome
```

Percent-encode reserved characters in credentials and database names.
Check SQLx support for any additional connection options. Store the complete URL
in a deployment secret. Supply it through `WALLET_PROXY_DATABASE_URL`.
Do not use a URL file or separate `DB_*` variables. `DATABASE_URL` is for tests and
query tools, not the service.

### Check unsupported settings

Do not replace Haskell if your deployment requires these unsupported options:

- `--grpc-target`: Rust rejects the node target. See the compatibility issues below.
- `--drop-account`, `--drop-amount`: no CCD-drop service.
- `--transak-config`: no Transak purchase-session service.

If you use `REPORTERS_JSON` and `K8S_POD_NAME`, select the node and database in the
deployment configuration. Rust does not perform this per-pod selection.

## Known compatibility issues

- **Update range validation:** Rust requires complete, valid range expressions.
  It rejects whitespace and trailing junk instead of accepting partial input or
  silently treating unrecognized input as an empty range set.

- **Node selection:** Haskell supports a gRPC `target` header. Rust rejects
  `--node-target` and `WALLET_PROXY_NODE_TARGET` because the SDK does not expose
  custom request metadata.
- **Node cookies:** Haskell forwards request cookies to the node and node
  `set-cookie` headers to the wallet. Rust does neither because the SDK does not
  expose the required metadata.
- **Malformed node responses:** During health checks, some SDK response-conversion
  failures cannot be distinguished from remote RPC errors. Rust can return HTTP
  200 with `healthy: false` where Haskell would return HTTP 500.

## Tests

```sh
cargo test --locked --all-targets
```

PostgreSQL tests are ignored by default because they create isolated databases.
Run them against a disposable server with a role permitted to create databases:

```sh
export DATABASE_URL=postgres://postgres:postgres@localhost:5432/wallet_proxy_test
cargo test --locked --all-targets -- --include-ignored
```

The [test schema](tests/fixtures/schema.sql) contains the health-query tables and
index. It is not a production migration. Integration tests cover empty/populated
tables, decoding/schema failures, SDK and HTTP startup, and catalogue responses.

## Database query checking

Application queries use SQLx compile-time checking and committed offline metadata.
Normal builds use `SQLX_OFFLINE=true` through Cargo configuration and do not need a
live database. CI checks the metadata against PostgreSQL; an offline build alone
does not establish database correctness.

To regenerate and check metadata, install SQLx CLI 0.9.0 and prepare a fresh,
disposable PostgreSQL database:

```sh
export DATABASE_URL=postgres://postgres:postgres@localhost:5432/wallet_proxy_test
cargo install sqlx-cli --version 0.9.0 --locked --no-default-features --features postgres,rustls
psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -f rust/tests/fixtures/schema.sql
SQLX_OFFLINE=false cargo sqlx prepare -- --all-targets
SQLX_OFFLINE=false cargo sqlx prepare --check -- --all-targets
```
