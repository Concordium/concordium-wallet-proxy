//! This module calculates the legacy token-update estimate from encoded sizes.
//! It accepts only operation names that the Haskell handler lists, including zero counts.

use super::{FeeError, calculation, contract::required, numbers, token_json};
use concordium_rust_sdk::base::transactions::cost;

/// Calculate a token estimate without encoding or invoking token operations.
pub(super) fn energy(
    query: &[(String, String)],
    signatures: u64,
    sponsored: bool,
) -> Result<u64, FeeError> {
    let size = numbers::int(required(query, "listOperationsSize")?)
        .ok_or_else(|| FeeError::Parse("Could not parse `listOperationsSize` value.".into()))?;
    let counts =
        token_json::counts(required(query, "tokenOperationTypeCount")?).map_err(|message| {
            FeeError::Parse(format!(
                "Could not parse `tokenOperationTypeCount` JSON: {}",
                token_json::show(&message)
            ))
        })?;
    let token = required(query, "tokenId")?;
    let mut operation_energy = 0u64;
    for (name, count) in counts {
        let operation = match name.as_str() {
            "transfer" => cost::PLT_TRANSFER,
            "mint" => cost::PLT_MINT,
            "burn" => cost::PLT_BURN,
            "addAllowList" | "removeAllowList" | "addDenyList" | "removeDenyList" => {
                cost::PLT_LIST_UPDATE
            }
            "pause" | "unpause" => cost::PLT_PAUSE,
            "assignAdminRoles" | "revokeAdminRoles" => cost::PLT_ASSIGN_REVOKE_ROLES,
            "updateMetadata" => cost::PLT_UPDATE_TOKEN_METADATA,
            _ => {
                let detail =
                    format!("Token operation specific cost for operation `{name}` is unknown.");
                return Err(FeeError::Parse(format!(
                    "Token operation specific cost error: {}",
                    token_json::show(&detail)
                )));
            }
        };
        operation_energy =
            operation_energy.wrapping_add(operation.energy.wrapping_mul(count as u64));
    }
    // Haskell adds Int sizes, then converts the result to a Word32 PayloadSize.
    let payload = size.wrapping_add(token.len() as i64).wrapping_add(6) as u32;
    Ok(calculation::minimum(payload, signatures, sponsored)
        .wrapping_add(cost::PLT_OPERATIONS_TRANSACTIONS.energy)
        .wrapping_add(operation_energy))
}
