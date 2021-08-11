#!/usr/bin/env bash

set -euxo pipefail

args=(
	--grpc-ip "${GRPC_HOST}"
	--grpc-port "${GRPC_PORT}"
	--ip-data "${IP_DATA_FILE}"
	--db "host=${DB_HOST} port=${DB_PORT} user=${DB_USER} dbname=${DB_NAME} password=${DB_PASSWORD}"
)
if [ -n "${DROP_ACCOUNT_FILE-}" ]; then
	args+=( --drop-account "${DROP_ACCOUNT_FILE}" )
fi

# Inherits env vars and args.
/wallet-proxy "${args[@]}" "${@}"
