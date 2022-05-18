#!/usr/bin/env bash

set -euxo pipefail

grpc_host="${GRPC_HOST}"
grpc_port="${GRPC_PORT}"
ip_data_file="${IP_DATA_FILE}"
db_host="${DB_HOST}"
db_port="${DB_PORT}"
db_user="${DB_USER}"
db_name="${DB_NAME}"
db_password="${DB_PASSWORD}"
drop_account_file="${DROP_ACCOUNT_FILE-}"
forced_update_config_file="${FORCED_UPDATE_CONFIG_FILE-}"

args=(
	--grpc-ip "${grpc_host}"
	--grpc-port "${grpc_port}"
	--ip-data "${ip_data_file}"
	--db "host=${db_host} port=${db_port} user=${db_user} dbname=${db_name} password=${db_password}"
)
if [ -n "${drop_account_file}" ]; then
	args+=( --drop-account "${drop_account_file}" )
fi
if [ -n "${forced_update_config_file}" ]; then
	args+=( --forced-update-config "${forced_update_config_file}" )
fi

# Inherits env vars and args.
/wallet-proxy "${args[@]}" "${@}"
