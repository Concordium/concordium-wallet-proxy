//! This module discovers CIS-2 contract tokens and reads metadata references and balances.
//! It preserves legacy binary formats and versioned batch-versus-partial invocation behavior.

mod codec;
mod routes;
pub use routes::routes;

use crate::network::NetworkFailure;
use concordium_rust_sdk::types::{
    ContractAddress,
    hashes::BlockHash,
    smart_contracts::{ContractContext, InvokeContractResult, OwnedContractName},
};
use num::BigInt;
use std::future::Future;

/// This record holds a token from the externally owned discovery table.
pub struct IndexedToken {
    /// This identifier is the exclusive discovery cursor.
    pub id: i64,
    /// These bytes retain the complete stored token identifier.
    pub token: Vec<u8>,
    /// This signed integer retains the full stored supply precision.
    pub supply: BigInt,
}

/// This interface reads indexed CIS-2 tokens without changing external tables.
pub trait Cis2History: Send + Sync {
    /// Read an ascending page for one exact contract address.
    fn contract_tokens(
        &self,
        address: ContractAddress,
        from: Option<i64>,
        limit: u16,
    ) -> impl Future<Output = anyhow::Result<Vec<IndexedToken>>> + Send;
}

/// This record pins a contract name and all invocations to the same finalized block.
pub struct ContractSnapshot {
    /// This hash comes from the instance-information response header.
    pub block: BlockHash,
    /// This name retains the initialization prefix supplied by the node.
    pub name: OwnedContractName,
}

/// This interface supplies read-only CIS-2 contract queries.
pub trait Cis2Node: Send + Sync {
    /// Read instance information at LastFinal and retain its response block hash.
    fn contract_snapshot(
        &self,
        address: ContractAddress,
    ) -> impl Future<Output = Result<ContractSnapshot, NetworkFailure>> + Send;
    /// Invoke a contract at the exact snapshot block with the supplied context.
    fn cis2_invoke(
        &self,
        block: BlockHash,
        context: &ContractContext,
    ) -> impl Future<Output = Result<InvokeContractResult, NetworkFailure>> + Send;
}
