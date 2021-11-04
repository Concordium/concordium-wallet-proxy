#!/usr/bin/env bash

set -euxo pipefail

# Get list of active reporter nodes and connection info from env.
reporters="${REPORTERS_JSON}"
pod_name="${K8S_POD_NAME}"

# Extract index from pod name (format: "<name>-<index>")
# by stripping the longest prefix matching the pattern "*-" from 'pod_name'.
index="${pod_name##*-}"

# Parse reporter IP and DB credentials based on the pod index (using round-robin on "enabled" entries).
# JSON format: "[{grpc: {host, port}, db: {host, name, port, user, password}, enabled}]".
reporter_args_tsv="$(jq -r "map(select(.enabled)) | .[${index} % (.|length)] | [.grpc.host, .grpc.port, .db.host, .db.port, .db.user, .db.name, .db.password] | @tsv" <<< "${reporters}")"
IFS=$'\t' read -r grpc_host grpc_port db_host db_port db_user db_name db_password <<< "${reporter_args_tsv}"

# Invoke default entrypoint - inherits env vars and args.
GRPC_HOST="${grpc_host}" \
GRPC_PORT="${grpc_port}" \
DB_HOST="${db_host}" \
DB_PORT="${db_port}" \
DB_USER="${db_user}" \
DB_NAME="${db_name}" \
DB_PASSWORD="${db_password}" \
/docker-entrypoint.sh "${@}"
