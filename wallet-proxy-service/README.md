# Wallet Proxy Service

Rust implementation of the Wallet Proxy API. The API is described
in [API.md](API.md)

## Running the service

To run the service (with minimal needed configuration):

```sh
service --node http://localhost:20000 --database-url postgres://postgres:password@localhost/wallet-proxy
```

To build from souce and run the service, replace `service` with `cargo run --bin service --`

## Configuration

The Wallet Proxy service have several options for configuration, and these can be provided as command-line arguments or/and environment variables, where the command-line arguments take precedence.

The required configurations are:

- `--database-url <url>` (env `WALLET_PROXY_DATABASE_URL=<url>`): where `<url>` is the database connection in the format of a URL ex. `postgres://user:password@localhost/walletproxy`.
- `--node <url>` (env `WALLET_PROXY_GRPC_ENDPOINTS=<url>`): where `<url>` is the gRPC endpoint of a Concordium node on the relevant network.
  Multiple nodes can be provided by providing multiple `--node <url>` arguments (environment variable take a comma separated list of URLs).

For full list of configuration options for the indexer service run:

```
wallet-proxy --help
```

## Querying the service

### Monitoring

Health and prometheus metrics are exposed as HTTP endpoints and can be queried like:
```sh
curl -XGET http://localhost:8003/health
curl -XGET http://localhost:8003/metrics
```

### REST API

The REST API is documented in [API.md](API.md)
