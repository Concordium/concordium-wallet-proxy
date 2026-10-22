# Wallet deployment fixtures

These JSON files copy the update configurations from the local infrastructure checkout:

- mainnet: `deployment-configs/mainnet/default/wallet-proxy-stack/values-blue.yaml`
- testnet: `deployment-configs/testnet/default/wallet-proxy-stack/values-green.yaml`
- stagenet: `deployment-configs/stagenet/default/wallet-proxy-stack/values-blue.yaml`

Only JSON formatting was changed. Tests use committed copies, not the infrastructure checkout or a running Haskell service. Cutoff expectations are recorded separately in the tests. URLs are checked against the copied configuration values.

The user approved strict range validation instead of exact legacy parser compatibility, provided current deployment configurations remain supported. Wildcards, inclusive upper bounds, forced precedence, and suggestion URL fallback are required by these fixtures. Partial parsing, whitespace, empty comma entries, reversed bounds, and integer overflow are not supported.
