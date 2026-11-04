//! This module signs drops and reads the legacy bare account transaction format.
//! Release timing and rounding follow the Haskell implementation.

use concordium_rust_sdk::{
    common::{Deserial, types::TransactionTime},
    id::types::{AccountAddress, AccountKeys},
    smart_contracts::common::{Amount, Timestamp},
    types::{
        Nonce,
        transactions::{AccountTransaction, EncodedPayload, construct},
    },
};
use rand::Rng;
use serde::Deserialize;
use std::{io::Cursor, path::Path};

/// These settings contain the funding address, signing keys, and drop amount.
#[derive(Deserialize)]
pub struct DropSettings {
    /// This account funds each new drop.
    pub address: AccountAddress,
    /// These credentials select the first threshold keys in ascending order.
    #[serde(rename = "accountKeys")]
    pub keys: AccountKeys,
    /// This amount is supplied by the startup option, not the key file.
    #[serde(skip)]
    pub amount: Amount,
}
impl DropSettings {
    /// Read the legacy account-key JSON without logging secret contents.
    pub fn load(path: &Path, amount: Amount) -> anyhow::Result<Self> {
        let bytes = std::fs::read(path)
            .map_err(|_| anyhow::anyhow!("Cannot read drop account key file"))?;
        let mut settings: Self = serde_json::from_slice(&bytes)
            .map_err(|_| anyhow::anyhow!("Invalid drop account key file"))?;
        settings.amount = amount;
        Ok(settings)
    }
    fn signatures(&self) -> u32 {
        self.keys
            .keys
            .values()
            .take(u8::from(self.keys.threshold) as usize)
            .map(|credential| {
                credential
                    .keys
                    .len()
                    .min(u8::from(credential.threshold) as usize) as u32
            })
            .sum()
    }
}

/// This source controls expiry and random release schedules.
pub trait DropTime: Send + Sync {
    /// Read POSIX seconds rounded to the nearest integer, with ties to even.
    fn seconds(&self) -> u64;
    /// Select a release count from 1 through 15, inclusive.
    fn releases(&self) -> u64;
}
/// This source uses system time and uniformly sampled release counts.
pub struct SystemDropTime;
impl DropTime for SystemDropTime {
    fn seconds(&self) -> u64 {
        let now = chrono::Utc::now();
        let seconds = now.timestamp();
        let nanos = now.timestamp_subsec_nanos();
        (seconds + i64::from(nanos > 500_000_000 || (nanos == 500_000_000 && seconds % 2 != 0)))
            as u64
    }
    fn releases(&self) -> u64 {
        rand::thread_rng().gen_range(1..=15)
    }
}

pub(super) fn decode(bytes: &[u8]) -> anyhow::Result<AccountTransaction<EncodedPayload>> {
    let mut cursor = Cursor::new(bytes);
    let transaction = AccountTransaction::deserial(&mut cursor)?;
    anyhow::ensure!(
        cursor.position() == bytes.len() as u64,
        "Trailing stored drop transaction bytes"
    );
    Ok(transaction)
}

pub(super) fn build(
    settings: &DropSettings,
    recipient: AccountAddress,
    nonce: Nonce,
    scheduled: bool,
    time: &impl DropTime,
) -> anyhow::Result<AccountTransaction<EncodedPayload>> {
    let now = time.seconds();
    let expiry = TransactionTime::from_seconds(
        now.checked_add(300)
            .ok_or_else(|| anyhow::anyhow!("Drop expiry overflow"))?,
    );
    let transaction = if scheduled {
        let count = time.releases();
        anyhow::ensure!((1..=15).contains(&count), "Invalid release count");
        let portion = settings.amount.micro_ccd / count;
        let remainder = settings.amount.micro_ccd % count;
        let mut schedule = Vec::with_capacity(count as usize);
        for index in 1..=count {
            let millis = now
                .checked_mul(1000)
                .and_then(|now| now.checked_add((index + 1) * 300_000))
                .ok_or_else(|| anyhow::anyhow!("Drop release time overflow"))?;
            schedule.push((
                Timestamp::from_timestamp_millis(millis),
                Amount::from_micro_ccd(portion + if index == 1 { remainder } else { 0 }),
            ));
        }
        construct::transfer_with_schedule(
            settings.signatures(),
            settings.address,
            nonce,
            expiry,
            recipient,
            schedule,
        )
    } else {
        construct::transfer(
            settings.signatures(),
            settings.address,
            nonce,
            expiry,
            recipient,
            settings.amount,
        )
    };
    Ok(transaction.sign(&settings.keys))
}
