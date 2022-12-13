#!/usr/bin/env bash

set -euxo pipefail

grpc_host="${GRPC_HOST}"
grpc_port="${GRPC_PORT}"
ip_data_file="${IP_DATA_FILE}"
ip_data_file_v1="${IP_DATA_FILE_V1}"
db_host="${DB_HOST}"
db_port="${DB_PORT}"
db_user="${DB_USER}"
db_name="${DB_NAME}"
db_password="${DB_PASSWORD}"
drop_account_file="${DROP_ACCOUNT_FILE-}"
forced_update_config_file_v0="${FORCED_UPDATE_CONFIG_FILE_V0-}"
forced_update_config_file_v1="${FORCED_UPDATE_CONFIG_FILE_V1-}"
use_tls="${USE_TLS:-false}" # if the variable is not set default to not enabling TLS
log_level="${LOG_LEVEL:-debug}" # default log level is debug
grpc_timeout="${GRPC_TIMEOUT:-15}"
grpc_retry="${GRPC_RETRY:-0}" # number of times to retry a failed request
tc_version="${TC_VERSION-}"
tc_url="${TC_URL-}"

args=(
	--grpc-ip "${grpc_host}"
	--grpc-port "${grpc_port}"
    --grpc-retry "${grpc_retry}"
	--ip-data "${ip_data_file}"
    --ip-data-v1 "${ip_data_file_v1}"
    --log-level "${log_level}"
    --grpc-timeout "${grpc_timeout}"
	--db "host=${db_host} port=${db_port} user=${db_user} dbname=${db_name} password=${db_password}"
    --tc-version "${tc_version}"
)
if [ -n "${tc_version}" ]; then
	args+=( --tc-version "${tc_version}" )
fi
if [ -n "${tc_url}" ]; then
	args+=( --tc-url "${tc_url}" )
fi
if [ -n "${drop_account_file}" ]; then
	args+=( --drop-account "${drop_account_file}" )
fi
if [ -n "${forced_update_config_file_v0}" ]; then
	args+=( --forced-update-config-v0 "${forced_update_config_file_v0}" )
fi
if [ -n "${forced_update_config_file_v1}" ]; then
	args+=( --forced-update-config-v1 "${forced_update_config_file_v1}" )
fi
if [ "${use_tls}" = "true" ]; then
	args+=( --secure )
fi

# Inherits env vars and args.
/wallet-proxy "${args[@]}" "${@}"
