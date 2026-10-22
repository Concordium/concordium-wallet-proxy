# 01: Start the configured service and report health

- **Type:** AFK
- **PRD coverage:** UC 24–25; applicable NFRs

## Parent

[Wallet Proxy PRD](../prd.md)

## What to build

Deliver the first runnable Rust vertical slice using Axum, Tokio, and concordium-rust-sdk: startup-validated configuration and the existing public health capability backed by the node and real PostgreSQL. Use one package with a testable library and thin executable, feature modules, narrow external interfaces, cloned SDK clients, and shared connection pools.

Support typed Clap flags and environment variables. Recommend the database URL environment variable for credentials; do not support database URL files. Flags take precedence over the same option's environment value; environment values take precedence over defaults. Require an explicit node endpoint through a flag or environment variable; do not provide a default. Relative paths start at the process working directory. Do not load an application configuration file. Preserve required initialization, cryptographic-parameter acquisition, node security/targeting options, health freshness rules, and dependency failures. Establish shared compatibility handling for errors, language selection, relevant HTTP behavior, and configurable CORS.

Establish checked SQLx queries, committed offline metadata, representative test-only legacy schema, independently reviewed source-derived HTTP fixtures, and CI. Do not create a separate compatibility spike.

## Acceptance criteria

- [ ] Valid configuration starts the independently runnable replacement; invalid configuration and failed required initialization prevent serving and provide actionable diagnostics.
- [ ] Clap validates required options and typed values; flags override environment values. A database URL is required through an environment variable or flag; help and validation errors do not disclose the URL.
- [ ] Health responses preserve existing node/database checks, freshness boundaries, statuses, and bodies, including dependency failure cases.
- [ ] HTTP compatibility expectations are derived from current Haskell behavior and reviewed examples, not replacement output; no Haskell comparison runner is used.
- [ ] Tests exercise real PostgreSQL and controlled node behavior without production effects; schema fixtures preserve relevant legacy types, constraints, and indexes.
- [ ] SQLx queries are compile-time checked; offline metadata supports database-free builds and CI validates it against the representative schema. Unchecked exceptions require approval.
- [ ] CI runs unit/HTTP/database tests, formatting, Clippy, and query-metadata checks without a public node.
- [ ] Both implementations remain separately runnable; existing Haskell files and unrelated local changes are preserved.
- [ ] Structured stderr diagnostics add no required telemetry service or new public endpoint.

## Implementation status

In progress. Startup/configuration, SDK and PostgreSQL adapters, health HTTP behavior,
source-derived fixtures, test-only schema, offline query metadata, and a Rust CI
workflow are implemented. Configuration now uses Clap flags and environment
variables, without TOML. The PostgreSQL pool limit is configurable and defaults
to ten connections; zero is rejected. Formatting, Clippy, the offline build, and sixteen
socket-free HTTP/configuration tests pass locally.

Not yet accepted: five SDK transport tests cannot bind loopback sockets in the
current execution environment; four PostgreSQL tests require a disposable server;
the schema-derived offline metadata still needs live PostgreSQL validation; and
fixtures require independent human review. The CI workflow has not yet been
observed running. SDK metadata/cookie and error-provenance compatibility issues
are documented in the Rust README and remain release-blocking. Development may
continue, but this task and full compatibility are not marked complete.

## Blocked by

None - can start immediately. Acceptance remains subject to the verification and
compatibility issues listed above.
