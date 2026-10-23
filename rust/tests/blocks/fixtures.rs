//! This module builds synthetic SDK values and separate raw protobuf responses.
//! Expected HTTP values are hand-written or selected from the original inputs.

use super::*;
use concordium_rust_sdk::{
    common::{from_bytes, to_bytes, types::Amount},
    id::types::AccountAddress,
    types::*,
    v2::Upward,
};
use std::io::Cursor;

/// This address identifies the sender in each account transaction.
pub const SENDER: AccountAddress = AccountAddress([1; 32]);
/// This address identifies the receiver.
pub const RECEIVER: AccountAddress = AccountAddress([0; 32]);

/// Build block metadata for slot-based or BFT consensus.
pub fn info(bft: bool) -> p::BlockInfo {
    p::BlockInfo {
        hash: Some(p::BlockHash { value: vec![1; 32] }),
        parent_block: Some(p::BlockHash { value: vec![2; 32] }),
        last_finalized_block: Some(p::BlockHash { value: vec![3; 32] }),
        state_hash: Some(p::StateHash { value: vec![4; 32] }),
        height: Some(p::AbsoluteBlockHeight {
            value: 9_007_199_254_740_995,
        }),
        genesis_index: Some(p::GenesisIndex { value: 2 }),
        era_block_height: Some(p::BlockHeight { value: 10 }),
        receive_time: Some(p::Timestamp { value: NEXT + 1 }),
        arrive_time: Some(p::Timestamp { value: NEXT + 10 }),
        slot_time: Some(p::Timestamp { value: NEXT }),
        slot_number: if bft {
            None
        } else {
            Some(p::Slot { value: 2 })
        },
        baker: Some(p::BakerId { value: 42 }),
        finalized: true,
        transaction_count: u32::MAX,
        transactions_energy_cost: Some(p::Energy { value: u64::MAX }),
        transactions_size: u32::MAX,
        protocol_version: if bft {
            p::ProtocolVersion::ProtocolVersion6 as i32
        } else {
            p::ProtocolVersion::ProtocolVersion5 as i32
        },
        round: bft.then_some(p::Round { value: 7 }),
        epoch: bft.then_some(p::Epoch { value: 8 }),
    }
}
/// Build a raw sponsored transfer for actual SDK transport tests.
pub fn raw_transfer() -> p::BlockItemSummary {
    p::BlockItemSummary {
        index: Some(p::block_item_summary::TransactionIndex { value: 9 }),
        energy_cost: Some(p::Energy { value: 37 }),
        hash: Some(p::TransactionHash { value: vec![4; 32] }),
        details: Some(p::block_item_summary::Details::AccountTransaction(
            p::AccountTransactionDetails {
                cost: Some(p::Amount { value: 10 }),
                sender: Some(p::AccountAddress { value: vec![1; 32] }),
                sponsor: Some(p::SponsorDetails {
                    sponsor: Some(p::AccountAddress { value: vec![0; 32] }),
                    cost: Some(p::Amount { value: u64::MAX }),
                }),
                effects: Some(p::AccountTransactionEffects {
                    effect: Some(p::account_transaction_effects::Effect::AccountTransfer(
                        p::account_transaction_effects::AccountTransfer {
                            amount: Some(p::Amount { value: u64::MAX }),
                            receiver: Some(p::AccountAddress { value: vec![0; 32] }),
                            memo: Some(p::Memo {
                                value: vec![0, 255],
                            }),
                        },
                    )),
                }),
            },
        )),
    }
}
/// Build a typed summary with independently controlled sender and sponsor costs.
pub fn account(effect: AccountTransactionEffects) -> BlockItemSummary {
    BlockItemSummary {
        index: TransactionIndex { index: 9 },
        energy_cost: Energy { energy: 37 },
        hash: [4; 32].into(),
        details: Upward::Known(BlockItemSummaryDetails::AccountTransaction(
            AccountTransactionDetails {
                cost: Amount::from_micro_ccd(10),
                sender: SENDER,
                sponsor: None,
                effects: Upward::Known(effect),
            },
        )),
    }
}
/// Build valid deterministic baker key events from synthetic keys.
pub fn baker_keys() -> BakerKeysEvent {
    let keys = super::super::balances::fixtures::keys();
    BakerKeysEvent {
        baker_id: BakerId {
            id: AccountIndex { index: 42 },
        },
        account: SENDER,
        sign_key: keys.signature_verify,
        election_key: keys.election_verify,
        aggregation_key: keys.aggregation_verify,
    }
}
/// Build an account credential identifier from a known compressed point.
pub fn credential() -> CredentialRegistrationID {
    from_bytes(&mut Cursor::new(super::super::balances::fixtures::point())).unwrap()
}
/// Build a zero encrypted amount from four compressed identity points.
pub fn encrypted() -> concordium_rust_sdk::encrypted_transfers::types::EncryptedAmount<
    concordium_rust_sdk::id::constants::ArCurve,
> {
    from_bytes(&mut Cursor::new(
        super::super::balances::fixtures::encrypted().value,
    ))
    .unwrap()
}
/// Build the removal event for an encrypted transfer.
pub fn removed() -> Box<EncryptedAmountRemovedEvent> {
    Box::new(EncryptedAmountRemovedEvent {
        account: SENDER,
        new_amount: encrypted(),
        input_amount: encrypted(),
        up_to_index: 2.into(),
    })
}
/// Build the receiving event for an encrypted transfer.
pub fn added() -> Box<NewEncryptedAmountEvent> {
    Box::new(NewEncryptedAmountEvent {
        receiver: RECEIVER,
        new_index: 3.into(),
        encrypted_amount: encrypted(),
    })
}
/// Render key bytes directly from synthetic inputs, not HTTP or SDK JSON output.
pub fn key_fields(keys: &BakerKeysEvent) -> Value {
    let hex = |bytes: Vec<u8>| {
        bytes
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>()
    };
    json!({"bakerId": 42, "account": expected("summary")["sender"], "signKey": hex(to_bytes(&keys.sign_key)), "electionKey": hex(to_bytes(&keys.election_key)), "aggregationKey": hex(to_bytes(&keys.aggregation_key))})
}
