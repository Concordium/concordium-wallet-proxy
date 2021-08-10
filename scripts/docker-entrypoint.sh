#!/usr/bin/env bash

set -euxo pipefail

reporters="${WALLET_PROXY_REPORTERS}"
index="${WALLET_PROXY_REPORTER_INDEX}"

drop_account="${WALLET_PROXY_ACCOUNT_FILE-}"
ip_data="${WALLET_SERVER_INFOS_FILE}"

# Read reporter IP and DB credentials from env based on the provided index.
reporter_args_tsv="$(jq -r ".[${index}] | [.ip, .grpc.port, .db.port, .db.user, .db.name, .db.password] | @tsv" <<< "${reporters}")"
IFS=$'\t' read -r ip grpc_port db_port db_user db_name db_password <<< "${reporter_args_tsv}"

args=(
	--grpc-ip "${ip}"
	--grpc-port "${grpc_port}"
	--db "host=${ip} port=${db_port} user=${db_user} dbname=${db_name} password=${db_password}"
	--ip-data "${ip_data}"
)
if [ -n "${drop_account}" ]; then
	args+=( --drop-account "${drop_account}" )
fi

# Inherits env vars and args.
/wallet-proxy "${args[@]}" "$@"
