# Wallet Proxy

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)
![Build and test](https://github.com/Concordium/concordium-node/actions/workflows/build-test.yaml/badge.svg)

This repository contains the Wallet Proxy, which works as middleware for the wallets maintained by 
Concordium. The Wallet Proxy acts as a proxy accessing the node and provides access to indexed chain data. 
Part of the Wallet Proxy is an indexer that indexes
chain data that is relevant for the wallets. 

Currently, there are both Haskell and Rust implementations. The Haskell implementation is fully functional but will be removed long term. 
The Rust implementation is still WIP.

- [wallet-proxy-indexer](./wallet-proxy-indexer/) (Rust, to be moved from <https://github.com/Concordium/concordium-transaction-logger>)
  Indexer of chain data. The indexed data is written to a Postgres database.
- [wallet-proxy-service](./wallet-proxy-service/) (Rust, WIP)
  Service that exposes the Wallet Proxy API. Reads data from the Postgres database
  or directly from the node. 
- [wallet-proxy](.) (Haskell)
  Service that exposes the Wallet Proxy API. Reads data from the Postgres database
  or directly from the node.

## Component Interaction Diagram

![Component Interaction Diagram](docs/diagrams/wallet-proxy.drawio.png)

## Submodules

The submodules in `deps` can be checked out using `git submodule update --init --recursive`.

# Deployment and Configuration (Haskell Wallet Proxy Service)

The wallet proxy must have access to
- a running node via its GRPC interface
- a transaction outcome database, a postgres database
- when it starts it needs to be given a file with the list of identity providers
  and anonymity revokers, and their public keys and metadata.
- optionally, private keys of the CCD drop account. If this is not provided then
  the gtu drop functionality will be disabled.
- optionally, the configuration file for the `v0/appSettings` endpoint. See
  [Forced update configuration file](#forced-update-configuration-file) for the format of the file.

An example invocation is
```console
wallet-proxy --grpc-ip 127.0.0.1\
             --grpc-port 10000\
             --db "host=localhost port=5432 dbname=transaction-outcome user=postgres password=password"\
             --ip-data identity-providers-with-metadata.json\
             --ip-data-v1 identity-providers-with-metadata-v1.json\
             --ip-data-v2 identity-providers-with-metadata-v2.json\
             --drop-account gtu-drop-account-0.json\
             --forced-update-config-v0 forced-update-config-v0.json\
             --forced-update-config-v1 forced-update-config-v1.json\
             --health-tolerance 30\
             --log-level debug\
             --grpc-timeout 15
```

or

```console
stack run wallet-proxy -- \
  --grpc-ip grpc.devnet-plt-beta.concordium.com \
  --grpc-port 20000\
  --db "host=localhost port=5432 dbname=transaction-outcome user=postgres password=password"\
  --ip-data ./examples/identity-providers-with-metadata.json\
  --ip-data-v1 ./examples/identity-providers-with-metadata-v1.json\
  --ip-data-v2 ./examples/identity-providers-with-metadata-v2.json\
  --log-level debug\
  --port 3005 \
  --secure
```

where
- `--grpc-ip 127.0.0.1` specifies the IP of the node
- `--grpc-port 10000` specifies the GRPC port the node is listening on
- `--db "host=localhost port=5432 dbname=transaction-outcome user=postgres password=postgres"` is the transaction outcome database connection string
- `--ip-data identity-providers-with-metadata.json` JSON file with identity providers, anonymity revokers and metadata needed for the version 0 identity flow
- `--ip-data-v1 identity-providers-with-metadata-v1.json` JSON file with identity providers and anonymity revokers and metadata needed for the version 1 identity flow
- `--ip-data-v2 identity-providers-with-metadata-v2.json` JSON file with identity providers (including company ID providers) and anonymity revokers and metadata needed for the version 1 identity flow
- `--drop-account gtu-drop-account-0.json` keys of the gtu drop account
- `--forced-update-config-v0 forced-update-config-v0.json` file with app update configuration for the old mobile wallet
- `--forced-update-config-v1 forced-update-config-v1.json` file with app update configuration for the new mobile wallet
- `--health-tolerance 30` tolerated age of last final block in seconds before the health query returns false
- `--log-level debug` means all logs above debug will be printed. Options are
  `off`, `warning`, `error`, `info`, `debug`, `trace`.
- `--grpc-timeout 15` means that all requests to the node must terminate in at
  most 15s, otherwise they will be cancelled.

If the node only supports TLS connections then the wallet-proxy must be started
with the `--secure` flag to enable its use.

## Identity providers metadata files

### For the version 0 identity issuance flow
This must be a valid JSON file which contains an array of JSON objects of the following form
```json
{
  "metadata": {
    "issuanceStart": "https://identity.provider/issuance-start",
    "icon": "base 64 encoded png image",
    "support": "<support@identity-provider.id>"
  },
  "ipInfo": {
    "ipIdentity": 0,
    "ipDescription": {
      "name": "Short name as it appears on the chain.",
      "url": "http/identity.provider",
      "description": "Free form description"
    },
    "ipVerifyKey": "...",
    "ipCdiVerifyKey": "74e905294a9377408d87ab4ddc4202731c4f971561eeaf423e82ae9509b8d057"
  },
  "arsInfos": {
    "1": {
      "arIdentity": 1,
      "arDescription": {
        "name": "AR-1",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201886a215138ec53d870e2878bbe731381927e08eaafe97003f6f4831f18e47c9ee8913c5f806064b57341785f0376af"
    },
    "2": {
      "arIdentity": 2,
      "arDescription": {
        "name": "AR-2",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201ac7295a21c3c687112f454c1d222d74e0d9cc9249b3c1eef58eb66a8a039c0decf3ea413a656f6f2dbebb497b7a527"
    }
  }
}
```

Where
- the `ipInfo` field is the contents of the `identity-provider-*.pub.json` files generated by the genesis tool, minus the outer versioning.
- the `arsInfos` field has the same format (minus the versioning) as the `anonymity_revokers.json` file generated by the genesis tool.
- the `metadata` field needs to be constructed manually based on the desired setup and in communication with partners.
  - the `issuanceStart` link is where the wallet submits the initial identity creation request.
  - the `icon` needs to be a base64 encoded png image that should be obtained from the relevant identity provider.
  - the `support` field must contain a valid support email of the identity provider.

### For the version 1 identity issuance flow
This must be a valid JSON file which contains an array of JSON objects of the following form
```json
{
  "metadata": {
    "issuanceStart": "https://identity.provider/issuance-start",
    "recoveryStart": "https://identity.provider/recovery-start",
    "icon": "base 64 encoded png image",
    "support": "<support@identity-provider.id>"
  },
  "ipInfo": {
    "ipIdentity": 0,
    "ipDescription": {
      "name": "Short name as it appears on the chain.",
      "url": "http/identity.provider",
      "description": "Free form description"
    },
    "ipVerifyKey": "...",
    "ipCdiVerifyKey": "74e905294a9377408d87ab4ddc4202731c4f971561eeaf423e82ae9509b8d057"
  },
  "arsInfos": {
    "1": {
      "arIdentity": 1,
      "arDescription": {
        "name": "AR-1",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201886a215138ec53d870e2878bbe731381927e08eaafe97003f6f4831f18e47c9ee8913c5f806064b57341785f0376af"
    },
    "2": {
      "arIdentity": 2,
      "arDescription": {
        "name": "AR-2",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201ac7295a21c3c687112f454c1d222d74e0d9cc9249b3c1eef58eb66a8a039c0decf3ea413a656f6f2dbebb497b7a527"
    }
  }
}
```

Where
- the `ipInfo` field is the contents of the `identity-provider-*.pub.json` files generated by the genesis tool, minus the outer versioning.
- the `arsInfos` field has the same format (minus the versioning) as the `anonymity_revokers.json` file generated by the genesis tool.
- the `metadata` field needs to be constructed manually based on the desired setup and in communication with partners.
  - the `issuanceStart` link is where the wallet submits the initial identity creation request.
  - the `issuanceRecovery` link is where the wallet submits the identity recovery request.
  - the `icon` needs to be a base64 encoded png image that should be obtained from the relevant identity provider.
  - the `support` field must contain a valid support email of the identity provider.

NB: It is OK to have the same identity provider listed multiple times in this file, i.e., the same identity provider could have two verification backends, in which case they would be listed twice in the list, the difference between the two instances being the `issuanceStart` and `icon` fields.

### For the version 2 identity issuance flow
This must be a valid JSON file which contains an array of JSON objects of the following form
```json
{
  "metadata": {
    "display": "Alternative display name",
    "issuanceStart": "https://identity.provider/issuance-start",
    "recoveryStart": "https://identity.provider/recovery-start",
    "icon": "base 64 encoded png image",
    "support": "<support@identity-provider.id>"
  },
  "ipInfo": {
    "ipIdentity": 0,
    "ipDescription": {
      "name": "Short name as it appears on the chain.",
      "url": "http/identity.provider",
      "description": "Free form description"
    },
    "ipVerifyKey": "...",
    "ipCdiVerifyKey": "74e905294a9377408d87ab4ddc4202731c4f971561eeaf423e82ae9509b8d057"
  },
  "arsInfos": {
    "1": {
      "arIdentity": 1,
      "arDescription": {
        "name": "AR-1",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201886a215138ec53d870e2878bbe731381927e08eaafe97003f6f4831f18e47c9ee8913c5f806064b57341785f0376af"
    },
    "2": {
      "arIdentity": 2,
      "arDescription": {
        "name": "AR-2",
        "url": "",
        "description": ""
      },
      "arPublicKey": "93fdc40bb8af4cb75caf8a53928d247be6285784b29578a06df312c28854c1bfac2fd0183967338b578772398d41201ac7295a21c3c687112f454c1d222d74e0d9cc9249b3c1eef58eb66a8a039c0decf3ea413a656f6f2dbebb497b7a527"
    }
  }
}
```

Where
- the `ipInfo` field is the contents of the `identity-provider-*.pub.json` files generated by the genesis tool, minus the outer versioning.
- the `arsInfos` field has the same format (minus the versioning) as the `anonymity_revokers.json` file generated by the genesis tool.
- the `metadata` field needs to be constructed manually based on the desired setup and in communication with partners.
  - the `issuanceStart` link is where the wallet submits the initial identity creation request.
  - the `issuanceRecovery` link is where the wallet submits the identity recovery request.
  - the `icon` needs to be a base64 encoded png image that should be obtained from the relevant identity provider.
  - the `support` field must contain a valid support email of the identity provider.
  - the `display` field is optional and is the name to display for this identity provider, this is useful for when the same ID provider is listed twice, to allow them to be distinguished.

NB: It is OK to have the same identity provider listed multiple times in this file, i.e., the same identity provider could have two verification backends, in which case they would be listed twice in the list, the difference between the two instances being the `issuanceStart` and `icon` fields.

## Database setup

The wallet-proxy needs access to the transaction logging database in the form of a PostgreSQL database.
It assumes the layout and semantics is as described in the [transaction logging notes](https://github.com/Concordium/concordium-transaction-logger#database-format).
The wallet-proxy queries the database in specific patterns. Every query filters by a single account/contract and narrows down by id.
Some queries additionally filter out per transaction type or by time.
To support efficient retrieval in the common cases it is necessary that the `ati` and `cti` tables have a primary key index on the joint `(account, id)` columns (and analogous columns `(index, subindex, id)` for the `cti` table).
The order of columns matters, since PostgreSQL will make best use of these indices if the queries have an equality constraint on the leading columns and a relational constraint on the remaining ones.

## Forced update configuration file

If the update configuration file for version `i` is not present then the `/v{i}/appSettings`
endpoint will always return `ok`. If the file is present it must be a valid JSON
file in the following format

```json
{
    "ios": {
        "forceUpdateVersions": "5,8,10-17",
        "suggestUpdateVersions": "9",
        "url": "link to app store",
        "suggestUrl": "link to the app store"
    },
    "android": {
        "forceUpdateVersions": "-64",
        "suggestUpdateVersions": "65-80",
        "url": "link to app store",
        "suggestUrl": "link to the app store"
    }
}
```
where both `ios` and `android` are optional and default to empty ranges if not
present.

The field
- `forceUpdateVersions` must be a string that denotes ranges of app versions
  that should be forced to update.
- `suggestUpdateVersions` must be a string that denotes ranges of app versions where
  the users should upgrade, but are not required to.

The `forceUpdateVersions` is matched first, so overlapping ranges are biased
towards the `needsUpdate` outcome.

The range format is fairly standard, e.g., `5,8,10-17` denotes app versions `5`,
`8`, and `10` to `17`, inclusive. Infinite ranges are also supported, e.g.,
`-64` denotes the range of versions `<= 64`, and `64-` denotes the range of
versions `>= 64`.

The `suggestUrl` field is optional, and if not present the default is taken to
be `url`. The `suggestUrl` value is used if the version mathches the suggested
update, but **not** the forced update.
