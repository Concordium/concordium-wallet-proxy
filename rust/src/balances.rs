//! This module serves the legacy and current CCD account balance routes.
//! It preserves the order and block selection of account and staking queries.

mod view;

use crate::{
    dependencies::Dependencies,
    http::error,
    network::{NetworkFailure, NetworkNode},
};
use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    types::{AccountInfo, AccountStakingInfo, BakerId, BakerPoolStatus, DelegationTarget},
    v2::{BlockIdentifier, Upward},
};
use serde_json::{Value, json};
use std::future::Future;

/// This interface supplies the account and staking queries required by balances.
pub trait BalanceNode: NetworkNode {
    /// Read account state at the specified block.
    fn account_info(
        &self,
        address: AccountAddress,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<AccountInfo, NetworkFailure>> + Send;
    /// Read validator pool state at the specified block.
    fn pool_info(
        &self,
        baker: BakerId,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<BakerPoolStatus, NetworkFailure>> + Send;
    /// Read the next payday at the specified block, if the protocol has paydays.
    fn next_payday(
        &self,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<Option<DateTime<Utc>>, NetworkFailure>> + Send;
    /// Read the reward period length in epochs, if the protocol has paydays.
    fn reward_period_epochs(
        &self,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<Option<u64>, NetworkFailure>> + Send;
}

/// This type supplies a directly cloned node adapter for balance requests.
pub struct Balances<D: Dependencies> {
    /// This adapter supplies independent node requests.
    pub node: D::Node,
}

impl<D: Dependencies> Clone for Balances<D> {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
        }
    }
}

/// This type selects the existing CCD response version.
#[derive(Clone, Copy, PartialEq)]
enum Version {
    /// This version includes both finalized and best-block state.
    Legacy,
    /// This version includes finalized state, cooldowns, and available balance.
    Current,
}

/// Build the two CCD balance routes. The token-inclusive version is separate.
pub fn routes<D: Dependencies + 'static>(balances: Balances<D>) -> Router
where
    D::Node: BalanceNode + 'static,
{
    Router::new()
        .route("/v0/accBalance/{address}", get(legacy::<D>))
        .route("/v1/accBalance/{address}", get(current::<D>))
        .with_state(balances)
}

async fn legacy<D: Dependencies>(
    State(balances): State<Balances<D>>,
    Path(address): Path<String>,
) -> Response
where
    D::Node: BalanceNode,
{
    balances.response(&address, Version::Legacy).await
}

async fn current<D: Dependencies>(
    State(balances): State<Balances<D>>,
    Path(address): Path<String>,
) -> Response
where
    D::Node: BalanceNode,
{
    balances.response(&address, Version::Current).await
}

/// This type joins an account to the pool state queried at the same block selector.
struct AccountState {
    /// This value contains the account state.
    account: AccountInfo,
    /// This value contains its validator or delegation-target pool, if applicable.
    pool: Option<BakerPoolStatus>,
}

impl<D: Dependencies> Balances<D>
where
    D::Node: BalanceNode,
{
    async fn response(&self, address: &str, version: Version) -> Response {
        let Ok(address) = address.parse::<AccountAddress>() else {
            return error(StatusCode::BAD_REQUEST, 1, "Malformed account address");
        };
        match self.query(address, version).await {
            Ok(value) => Json(value).into_response(),
            Err(failure) => failure.response(),
        }
    }

    async fn account(
        &self,
        address: AccountAddress,
        block: BlockIdentifier,
    ) -> Result<AccountState, NetworkFailure> {
        let account = self.node.account_info(address, block).await?;
        view::validate(&account)?;
        let baker = match account.account_stake.as_ref() {
            Some(Upward::Known(AccountStakingInfo::Baker { baker_info, .. })) => {
                Some(baker_info.baker_id)
            }
            Some(Upward::Known(AccountStakingInfo::Delegated {
                delegation_target: DelegationTarget::Baker { baker_id },
                ..
            })) => Some(*baker_id),
            Some(Upward::Unknown(_)) => return Err(NetworkFailure::InvalidResponse),
            _ => None,
        };
        let pool = match baker {
            Some(baker) => {
                let pool = self.node.pool_info(baker, block).await?;
                if pool
                    .active_baker_pool_status
                    .as_ref()
                    .is_some_and(|active| {
                        matches!(active.pool_info.open_status, Upward::Unknown(_))
                    })
                {
                    return Err(NetworkFailure::InvalidResponse);
                }
                Some(pool)
            }
            None => None,
        };
        Ok(AccountState { account, pool })
    }

    async fn query(
        &self,
        address: AccountAddress,
        version: Version,
    ) -> Result<Value, NetworkFailure> {
        let consensus = self.node.consensus_info().await?;
        let finalized = BlockIdentifier::Given(consensus.last_finalized_block);
        // Haskell suppresses NOT_FOUND around each entire account-and-pool query.
        // A missing finalized account does not cause a best-block lookup.
        let last = match self.account(address, finalized).await {
            Err(failure) if failure.is_not_found() => return Ok(json!({})),
            result => result?,
        };
        let best = if version == Version::Legacy {
            match self.account(address, BlockIdentifier::Best).await {
                Err(failure) if failure.is_not_found() => return Ok(json!({})),
                result => Some(result?),
            }
        } else {
            None
        };
        let next = self.node.next_payday(finalized).await?;
        let pending = view::has_pending(&last.account)
            || best
                .as_ref()
                .is_some_and(|state| view::has_pending(&state.account));
        let epochs = if pending {
            self.node.reward_period_epochs(finalized).await?
        } else {
            None
        };
        let payday = view::Payday {
            next,
            epoch_duration: consensus.epoch_duration,
            epochs,
        };
        let mut value = json!({"finalizedBalance": view::balance(&last.account, last.pool.as_ref(), version, &payday)?});
        if let Some(best) = best {
            value["currentBalance"] =
                view::balance(&best.account, best.pool.as_ref(), version, &payday)?;
        }
        Ok(value)
    }
}
