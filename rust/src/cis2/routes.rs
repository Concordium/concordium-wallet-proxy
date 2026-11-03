//! This module serves the existing CIS-2 discovery, metadata, and balance routes.
//! Partial versions omit contract-level failures but never hide node-query failures.

use super::{Cis2History, Cis2Node, codec};
use crate::{
    compatibility::{input, input::hex_encode as hex, numbers},
    http::error,
    network::NetworkFailure,
};
use axum::{
    Json, Router,
    extract::{Path, Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    types::{
        ContractAddress, Energy,
        smart_contracts::{
            ContractContext, InvokeContractResult, OwnedParameter, OwnedReceiveName,
        },
    },
};
use serde_json::{Value, json};
use std::sync::Arc;

/// Build all five contract-token routes with shared node and history adapters.
pub fn routes<N: Cis2Node + 'static, H: Cis2History + 'static>(
    node: Arc<N>,
    history: Arc<H>,
) -> Router {
    Router::new()
        .route("/v0/CIS2Tokens/{index}/{subindex}", get(discover::<N, H>))
        .route(
            "/v0/CIS2TokenMetadata/{index}/{subindex}",
            get(metadata::<N, H, false>),
        )
        .route(
            "/v1/CIS2TokenMetadata/{index}/{subindex}",
            get(metadata::<N, H, true>),
        )
        .route(
            "/v0/CIS2TokenBalance/{index}/{subindex}/{account}",
            get(balance::<N, H, false>),
        )
        .route(
            "/v1/CIS2TokenBalance/{index}/{subindex}/{account}",
            get(balance::<N, H, true>),
        )
        .with_state((node, history))
}

/// This mode selects invocation encoding and response projection.
#[derive(Clone, Copy)]
enum Mode {
    /// Read token metadata references.
    Metadata,
    /// Read balances for the supplied account without resolving its aliases.
    Balance(
        /// This address is serialized verbatim in each query pair.
        AccountAddress,
    ),
}

async fn discover<N: Cis2Node, H: Cis2History>(
    State((_, history)): State<(Arc<N>, Arc<H>)>,
    Path((index, subindex)): Path<(String, String)>,
    Query(query): Query<Vec<(String, String)>>,
) -> Response {
    let Some(address) = contract(&index, &subindex) else {
        return not_found();
    };
    let limit = first(&query, "limit")
        .and_then(numbers::int)
        .unwrap_or(20)
        .clamp(0, 1000) as u16;
    let from = first(&query, "from").and_then(|text| text.parse::<i64>().ok());
    match history.contract_tokens(address, from, limit).await {
        Ok(rows) => {
            let tokens: Vec<_> = rows.into_iter().map(|row| json!({"id":row.id,"token":hex(&row.token),"totalSupply":row.supply.to_string()})).collect();
            let mut body = json!({"limit":limit,"count":tokens.len(),"tokens":tokens});
            if let Some(from) = from {
                body["from"] = json!(from);
            }
            Json(body).into_response()
        }
        Err(failure) => {
            tracing::warn!(%failure,"CIS-2 discovery query failed");
            error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            )
        }
    }
}

async fn metadata<N: Cis2Node, H: Cis2History, const PARTIAL: bool>(
    State((node, _)): State<(Arc<N>, Arc<H>)>,
    Path((index, subindex)): Path<(String, String)>,
    Query(query): Query<Vec<(String, String)>>,
) -> Response {
    let Some(address) = contract(&index, &subindex) else {
        return not_found();
    };
    let tokens = match tokens(&query, PARTIAL) {
        Ok(tokens) => tokens,
        Err(message) => return parse_error(message),
    };
    read(node.as_ref(), address, tokens, Mode::Metadata, PARTIAL).await
}

async fn balance<N: Cis2Node, H: Cis2History, const PARTIAL: bool>(
    State((node, _)): State<(Arc<N>, Arc<H>)>,
    Path((index, subindex, account)): Path<(String, String, String)>,
    Query(query): Query<Vec<(String, String)>>,
) -> Response {
    let Some(address) = contract(&index, &subindex) else {
        return not_found();
    };
    let account = match input::account(&account) {
        Ok(account) => account,
        Err(message) => return parse_error(message),
    };
    let tokens = match tokens(&query, PARTIAL) {
        Ok(tokens) => tokens,
        Err(message) => return parse_error(message),
    };
    read(
        node.as_ref(),
        address,
        tokens,
        Mode::Balance(account),
        PARTIAL,
    )
    .await
}

/// Read finalized instance information once, then invoke only at that response block.
async fn read<N: Cis2Node>(
    node: &N,
    address: ContractAddress,
    tokens: Vec<Vec<u8>>,
    mode: Mode,
    partial: bool,
) -> Response {
    let snapshot = match node.contract_snapshot(address).await {
        Ok(snapshot) => snapshot,
        Err(failure) => return node_error(failure),
    };
    let name = snapshot.name.as_contract_name().contract_name();
    let entrypoint = match mode {
        Mode::Metadata => "tokenMetadata",
        Mode::Balance(_) => "balanceOf",
    };
    let groups: Vec<&[Vec<u8>]> = if partial {
        tokens.iter().map(std::slice::from_ref).collect()
    } else {
        vec![tokens.as_slice()]
    };
    let mut output = Vec::new();
    for group in groups {
        let context = ContractContext {
            invoker: None,
            contract: address,
            amount: concordium_rust_sdk::common::types::Amount::zero(),
            method: OwnedReceiveName::new_unchecked(format!("{name}.{entrypoint}")),
            parameter: OwnedParameter::new_unchecked(codec::parameter(
                group,
                match mode {
                    Mode::Balance(account) => Some(account),
                    _ => None,
                },
            )),
            energy: Some(Energy {
                energy: if partial { 50_000 } else { 500_000 },
            }),
        };
        let result = match node.cis2_invoke(snapshot.block, &context).await {
            Ok(result) => result,
            Err(failure) => return node_error(failure),
        };
        let bytes = match result {
            InvokeContractResult::Success {
                return_value: Some(value),
                ..
            } => value.value,
            InvokeContractResult::Success {
                return_value: None, ..
            } if !partial => {
                return error(
                    StatusCode::BAD_REQUEST,
                    1,
                    "Invoking a V0 contract is not supported.",
                );
            }
            _ if partial => continue,
            _ => return invoke_error(),
        };
        match project(&bytes, group, mode, partial) {
            Some(values) => output.extend(values),
            None if partial => {}
            None => return invoke_error(),
        }
    }
    Json(match mode {
        Mode::Metadata => json!({"contractName":name,"metadata":output}),
        Mode::Balance(_) => json!(output),
    })
    .into_response()
}

/// Project every declared return item before pairing it with the request IDs.
fn project(bytes: &[u8], tokens: &[Vec<u8>], mode: Mode, partial: bool) -> Option<Vec<Value>> {
    match mode {
        Mode::Metadata => Some(tokens.iter().zip(codec::metadata(bytes,partial.then_some(1))?).map(|(token,metadata)| json!({"tokenId":hex(token),"metadataURL":metadata.url,"metadataChecksum":metadata.checksum.map(|checksum| hex(&checksum))})).collect()),
        Mode::Balance(_) => Some(tokens.iter().zip(codec::balances(bytes,tokens.len())?).map(|(token,balance)| json!({"tokenId":hex(token),"balance":balance.to_string()})).collect()),
    }
}

/// Validate all IDs before applying the array and partial-version count limits.
fn tokens(query: &[(String, String)], partial: bool) -> Result<Vec<Vec<u8>>, &'static str> {
    let text = first(query, "tokenId").ok_or("Missing `tokenId` value.")?;
    let tokens = text
        .split(',')
        .map(|text| {
            input::hex(text).map_err(
                |_| "Error in $: expected Not a valid Base16 encoding., but encountered String",
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    if tokens.len() > usize::from(u16::MAX) {
        return Err("Too many token ids.");
    }
    if partial && tokens.len() > 20 {
        return Err("Too many tokens requested.");
    }
    Ok(tokens)
}

/// Read the first query value without treating empty text as missing.
fn first<'a>(query: &'a [(String, String)], name: &str) -> Option<&'a str> {
    query
        .iter()
        .find(|(key, _)| key == name)
        .map(|(_, value)| value.as_str())
}
/// Read bounded Word64 path components with the legacy signed-decimal rules.
fn contract(index: &str, subindex: &str) -> Option<ContractAddress> {
    Some(ContractAddress {
        index: input::path_word(index)?,
        subindex: input::path_word(subindex)?,
    })
}
/// Retain the custom NOT_FOUND message without hiding other node failures.
fn node_error(failure: NetworkFailure) -> Response {
    if failure.is_not_found() {
        error(StatusCode::NOT_FOUND, 2, "Not found")
    } else {
        failure.response()
    }
}
/// Format a malformed input response with the existing parser prefix.
fn parse_error(message: &str) -> Response {
    error(
        StatusCode::BAD_REQUEST,
        1,
        &format!("Parsing input failed: {message}"),
    )
}
/// Format a route mismatch before any feature query.
fn not_found() -> Response {
    error(StatusCode::NOT_FOUND, 1, "Not found")
}
/// Format contract failure and malformed return-data responses.
fn invoke_error() -> Response {
    error(StatusCode::BAD_REQUEST, 1, "Invoking a contract failed.")
}

#[cfg(test)]
mod tests {
    //! These tests check array bounds beyond the HTTP URI representation limit.

    use super::tokens;

    #[test]
    fn array_count_limits_follow_full_token_validation() {
        let query = |text: String| vec![("tokenId".to_owned(), text)];
        assert_eq!(
            tokens(&query(vec![""; 20].join(",")), true).unwrap().len(),
            20
        );
        assert_eq!(
            tokens(&query(vec![""; 21].join(",")), true).unwrap_err(),
            "Too many tokens requested."
        );
        assert_eq!(
            tokens(&query(vec![""; 65_535].join(",")), false)
                .unwrap()
                .len(),
            65_535
        );
        assert_eq!(
            tokens(&query(vec![""; 65_536].join(",")), false).unwrap_err(),
            "Too many token ids."
        );
        assert_eq!(
            tokens(&query(format!("{},gg", vec![""; 65_535].join(","))), true).unwrap_err(),
            "Error in $: expected Not a valid Base16 encoding., but encountered String"
        );
    }
}
