#!/usr/bin/env bash

set -euxo pipefail

# TODO Read GRPC_IP and DB credentials from env/file...

args=()
if [ -n "${WALLET_PROXY_GRPC_IP}" ]; then
	args+=( --grpc-ip "${WALLET_PROXY_GRPC_IP}" )
fi
if [ -n "${WALLET_PROXY_GRPC_PORT}" ]; then
	args+=( --grpc-port "${WALLET_PROXY_GRPC_PORT}" )
fi
if [ -n "${WALLET_PROXY_DATABASE}" ]; then
	args+=( --db "${WALLET_PROXY_DATABASE}" )
fi
if [ -n "${WALLET_PROXY_ACCOUNT_FILE}" ]; then
	args+=( --drop-account "${WALLET_PROXY_ACCOUNT_FILE}" )
fi
if [ -n "${WALLET_SERVER_INFOS_FILE}" ]; then
	args+=( --ip-data "${WALLET_SERVER_INFOS_FILE}" )
else
	args+=( --ip-data '/wallet-proxy-data/identity-providers-with-metadata.json' )
fi

# Inherits env vars and args.
/wallet-proxy "${args[@]}" "$@"
