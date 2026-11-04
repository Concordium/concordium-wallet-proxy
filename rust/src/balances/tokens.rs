//! This module builds the enriched token entries used by account balance v2.
//! It selects explicit HTTP fields instead of serializing SDK composite types.

use super::token_state;
use concordium_rust_sdk::protocol_level_tokens::{AccountToken, TokenInfo};
use serde_json::{Value, json};

pub(super) fn holding(holding: &AccountToken, info: &TokenInfo) -> Value {
    let mut account = json!({"balance": {
        "value":holding.state.balance.value().to_string(), "decimals":holding.state.balance.decimals()
    }});
    if let Some(state) = &holding.state.module_state {
        account["state"] = token_state::account(state.as_ref());
    }
    json!({"token": information(info), "tokenAccountState":account})
}

/// Select the global token fields used by the HTTP API.
pub(crate) fn information(info: &TokenInfo) -> Value {
    json!({
        "tokenId":info.token_id.to_string(),
        "tokenState": {
            "tokenModuleRef":info.token_state.token_module_ref.to_string(),
            "decimals":info.token_state.decimals,
            "totalSupply":{"value":info.token_state.total_supply.value().to_string(),"decimals":info.token_state.total_supply.decimals()},
            "moduleState":token_state::module(info.token_state.module_state.as_ref())
        }
    })
}
