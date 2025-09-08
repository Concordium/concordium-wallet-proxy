# Wallet Proxy Service

Rust implementation of the Wallet Proxy API. The API is described
in [API.md](API.md)

## Running the service

TBD

## Configuration

The Wallet Proxy service have several options for configuration, and these can be provided as command-line arguments or/and environment variables, where the command-line arguments take precedence.

The required configurations are:

- `--database-url <url>` (env `WALLET_PROXY_DATABASE_URL=<url>`): where `<url>` is the database connection in the format of a URL ex. `postgres://user:password@localhost/walletproxy`.
- `--node <url>` (env `WALLET_PROXY_GRPC_ENDPOINTS=<url>`): where `<url>` is the gRPC endpoint of a Concordium node on the relevant network.
  Multiple nodes can be provided by providing multiple `--node <url>` arguments (environment variable take a comma separated list of URLs).

For full list of configuration options for the indexer service run:

```
ccdscan-indexer --help
```


## Querying the service

TBD