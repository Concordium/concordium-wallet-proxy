#!/usr/bin/env bash

if [ -n "$WALLET_PROXY_GRPC_IP" ];
then
    ARGS="$ARGS --grpc-ip $WALLET_PROXY_GRPC_IP"
fi
if [ -n "$WALLET_PROXY_GRPC_PORT" ];
then
    ARGS="$ARGS --grpc-port $WALLET_PROXY_GRPC_PORT"
fi
if [ -n "$WALLET_PROXY_DATABASE" ];
then
    ARGS="$ARGS --db $WALLET_PROXY_DATABASE"
fi
if [ -n "$WALLET_PROXY_ACCOUNT_FILE" ];
then
    ARGS="$ARGS --drop-account $WALLET_PROXY_ACCOUNT_FILE"
fi
if [ -n "$DB_SLEEP" ];
then
    echo "Sleeping for $DB_SLEEP"
    sleep $DB_SLEEP
fi

eval "/wallet-proxy$ARGS"
