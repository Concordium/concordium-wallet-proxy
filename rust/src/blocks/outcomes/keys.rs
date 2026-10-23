//! This module projects governance key-update versions supported by Haskell.
//! It omits newer SDK authorization options and retains the legacy createPLT key.

use super::{encode, select};
use crate::network::NetworkFailure;
use concordium_rust_sdk::types::{AuthorizationsV1, Level1Update, RootUpdate};
use serde_json::{Value, json};

fn authorization_v0(
    keys: &concordium_rust_sdk::types::AuthorizationsV0,
) -> Result<Value, NetworkFailure> {
    select(
        keys,
        &[
            "keys",
            "emergency",
            "protocol",
            "electionDifficulty",
            "euroPerEnergy",
            "microGTUPerEuro",
            "foundationAccount",
            "mintDistribution",
            "transactionFeeDistribution",
            "paramGASRewards",
            "poolParameters",
            "addAnonymityRevoker",
            "addIdentityProvider",
        ],
    )
}

fn authorizations(keys: &AuthorizationsV1, create: bool) -> Result<Value, NetworkFailure> {
    let mut value = authorization_v0(&keys.v0)?;
    value["cooldownParameters"] = encode(&keys.cooldown_parameters)?;
    value["timeParameters"] = encode(&keys.time_parameters)?;
    if create {
        value["createPLT"] = encode(
            keys.create_plt
                .as_ref()
                .ok_or(NetworkFailure::InvalidResponse)?,
        )?;
    }
    Ok(value)
}
pub(super) fn root(value: &RootUpdate) -> Result<Value, NetworkFailure> {
    let (kind, payload) = match value {
        RootUpdate::RootKeysUpdate(keys) => {
            ("rootKeysUpdate", select(keys, &["keys", "threshold"])?)
        }
        RootUpdate::Level1KeysUpdate(keys) => {
            ("level1KeysUpdate", select(keys, &["keys", "threshold"])?)
        }
        RootUpdate::Level2KeysUpdate(keys) => ("level2KeysUpdate", authorization_v0(keys)?),
        RootUpdate::Level2KeysUpdateV1(keys) => {
            ("level2KeysUpdateV1", authorizations(keys, false)?)
        }
        RootUpdate::Level2KeysUpdateV2(keys) => ("level2KeysUpdateV2", authorizations(keys, true)?),
        RootUpdate::Level2KeysUpdateV3(_) => return Err(NetworkFailure::InvalidResponse),
    };
    Ok(json!({"typeOfUpdate":kind,"updatePayload":payload}))
}
pub(super) fn level1(value: &Level1Update) -> Result<Value, NetworkFailure> {
    let (kind, payload) = match value {
        Level1Update::Level1KeysUpdate(keys) => {
            ("level1KeysUpdate", select(keys, &["keys", "threshold"])?)
        }
        Level1Update::Level2KeysUpdate(keys) => ("level2KeysUpdate", authorization_v0(keys)?),
        Level1Update::Level2KeysUpdateV1(keys) => {
            ("level2KeysUpdateV1", authorizations(keys, false)?)
        }
        Level1Update::Level2KeysUpdateV2(keys) => {
            ("level2KeysUpdateV2", authorizations(keys, true)?)
        }
        Level1Update::Level2KeysUpdateV3(_) => return Err(NetworkFailure::InvalidResponse),
    };
    Ok(json!({"typeOfUpdate":kind,"updatePayload":payload}))
}
