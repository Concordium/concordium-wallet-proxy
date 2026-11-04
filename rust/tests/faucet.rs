//! This module checks drop HTTP behavior, persistence, and signed transaction contents.
//! Tests use fixed time, fixed release counts, and synthetic signing keys.

mod postgres;
mod retry;
mod signing;

use axum::{
    body::{Body, to_bytes},
    http::Request,
};
use concordium_rust_sdk::{
    common::{from_bytes, to_bytes as encode},
    id::types::AccountAddress,
    smart_contracts::common::Amount,
    types::{
        Nonce,
        hashes::TransactionHash,
        transactions::{AccountTransaction, EncodedPayload},
    },
};
use concordium_wallet_proxy::{
    faucet::{self, DropNode, DropSettings, DropStore, DropTime, Faucet, Record},
    network::NetworkFailure,
    submission::Submission,
};
use serde_json::{Value, json};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicU64, Ordering},
};
use tower::ServiceExt;

/// This recipient is distinct from the funding account.
const RECIPIENT: AccountAddress = AccountAddress([8; 32]);
/// This address identifies the funding account.
const SENDER: AccountAddress = AccountAddress([7; 32]);
/// This type keeps test transactions in their persisted account-only representation.
type Transaction = AccountTransaction<EncodedPayload>;

/// This state records operations and supplies controlled node outcomes.
struct Node {
    /// These calls expose query precedence and submission order.
    calls: Mutex<Vec<String>>,
    /// These bytes contain each submitted bare account transaction.
    submissions: Mutex<Vec<Vec<u8>>>,
    /// This flag indicates that the persisted transaction can still succeed.
    acceptable: bool,
    /// This amount is the recipient balance returned by finalized queries.
    balance: u64,
    /// This nonce is the recipient's finalized nonce.
    recipient_nonce: u64,
    /// This operation returns a transport error when selected.
    fail: Option<&'static str>,
    /// This flag makes the recipient absent from finalized state.
    not_final: bool,
}
impl Default for Node {
    fn default() -> Self {
        Self {
            calls: Mutex::default(),
            submissions: Mutex::default(),
            acceptable: true,
            balance: 3_000_000_000,
            recipient_nonce: 0,
            fail: None,
            not_final: false,
        }
    }
}
impl Node {
    fn record(&self, call: &str) -> Result<(), NetworkFailure> {
        self.calls.lock().unwrap().push(call.into());
        if self.fail == Some(call) {
            Err(NetworkFailure::Transport)
        } else {
            Ok(())
        }
    }
}
impl DropNode for Node {
    async fn account(&self, address: AccountAddress) -> Result<(Nonce, Amount), NetworkFailure> {
        assert_eq!(
            address, RECIPIENT,
            "The retry balance and nonce check must use the recipient"
        );
        self.record("account")?;
        if self.not_final {
            return Err(NetworkFailure::Status(Box::new(tonic::Status::not_found(
                "not finalized",
            ))));
        }
        Ok((
            Nonce::from(self.recipient_nonce),
            Amount::from_micro_ccd(self.balance),
        ))
    }
    async fn nonce(&self, address: AccountAddress) -> Result<Nonce, NetworkFailure> {
        assert_eq!(address, SENDER);
        self.record("nonce")?;
        Ok(Nonce::from(10))
    }
    async fn acceptable(&self, _: TransactionHash) -> Result<bool, NetworkFailure> {
        self.record("status")?;
        Ok(self.acceptable)
    }
    async fn submit(&self, item: &Submission) -> Result<(), NetworkFailure> {
        self.record("submit")?;
        let Submission::AccountTransaction(transaction) = item else {
            panic!("Expected account transaction");
        };
        self.submissions.lock().unwrap().push(encode(transaction));
        Ok(())
    }
}

/// This store models the recipient uniqueness constraint and row-specific deletion.
#[derive(Default)]
struct Store {
    /// This record holds the single controlled recipient's transaction.
    row: Mutex<Option<Record>>,
    /// These identifiers increase after each successful insert.
    next_id: AtomicU64,
    /// These calls expose write-before-submit ordering.
    calls: Mutex<Vec<&'static str>>,
    /// This flag injects a storage failure.
    fail: bool,
    /// This flag simulates a competing insert winning before this insert.
    conflict: bool,
}
impl DropStore for Store {
    async fn lookup(&self, address: AccountAddress) -> anyhow::Result<Option<Record>> {
        assert_eq!(address, RECIPIENT);
        anyhow::ensure!(!self.fail, "controlled storage failure");
        self.calls.lock().unwrap().push("lookup");
        Ok(self.row.lock().unwrap().as_ref().map(|row| Record {
            id: row.id,
            transaction: row.transaction.clone(),
        }))
    }
    async fn insert(&self, address: AccountAddress, transaction: &[u8]) -> anyhow::Result<bool> {
        assert_eq!(address, RECIPIENT);
        self.calls.lock().unwrap().push("insert");
        let mut row = self.row.lock().unwrap();
        if row.is_some() {
            return Ok(false);
        }
        if self.conflict {
            *row = Some(Record {
                id: 42,
                transaction: include_bytes!("fixtures/faucet/scheduled.bin").to_vec(),
            });
            return Ok(false);
        }
        *row = Some(Record {
            id: self.next_id.fetch_add(1, Ordering::SeqCst) as i64,
            transaction: transaction.into(),
        });
        Ok(true)
    }
    async fn delete(&self, id: i64) -> anyhow::Result<()> {
        self.calls.lock().unwrap().push("delete");
        let mut row = self.row.lock().unwrap();
        if row.as_ref().is_some_and(|row| row.id == id) {
            *row = None;
        }
        Ok(())
    }
}
/// This source fixes time and release count for deterministic signatures.
struct Time {
    /// This value is the rounded POSIX second.
    now: u64,
    /// This count controls the scheduled transfer split.
    count: u64,
}
impl DropTime for Time {
    fn seconds(&self) -> u64 {
        self.now
    }
    fn releases(&self) -> u64 {
        self.count
    }
}

fn settings() -> DropSettings {
    let mut settings: DropSettings = serde_json::from_value(json!({"address":SENDER,"accountKeys":{"threshold":1,"keys":{"0":{"threshold":1,"keys":{"0":{"signKey":"9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60","verifyKey":"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"}}}}}})).unwrap();
    settings.amount = Amount::from_micro_ccd(2_000_000_001);
    settings
}
fn service(
    node: Arc<Node>,
    store: Arc<Store>,
    now: u64,
    count: u64,
) -> Arc<Faucet<Node, Store, Time>> {
    Arc::new(Faucet {
        settings: Some(settings()),
        node,
        store,
        time: Time { now, count },
    })
}
async fn request<N: DropNode + 'static, S: DropStore + 'static, T: DropTime + 'static>(
    service: Arc<Faucet<N, S, T>>,
    body: &str,
) -> (u16, Value) {
    request_path(service, &RECIPIENT.to_string(), body).await
}
async fn request_path<N: DropNode + 'static, S: DropStore + 'static, T: DropTime + 'static>(
    service: Arc<Faucet<N, S, T>>,
    address: &str,
    body: &str,
) -> (u16, Value) {
    let response = faucet::routes(service)
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri(format!("/v0/testnetGTUDrop/{address}"))
                .body(Body::from(body.to_owned()))
                .unwrap(),
        )
        .await
        .unwrap();
    let status = response.status().as_u16();
    (
        status,
        serde_json::from_slice(&to_bytes(response.into_body(), usize::MAX).await.unwrap()).unwrap(),
    )
}
fn saved(store: &Store) -> Transaction {
    from_bytes(&mut std::io::Cursor::new(
        &store.row.lock().unwrap().as_ref().unwrap().transaction,
    ))
    .unwrap()
}

#[tokio::test]
async fn disabled_route_precedes_address_and_body_validation() {
    let node = Arc::new(Node::default());
    let service = Arc::new(Faucet {
        settings: None,
        node: node.clone(),
        store: Arc::new(Store::default()),
        time: Time {
            now: 1000,
            count: 1,
        },
    });
    assert_eq!(
        request_path(service, "invalid", "null").await,
        (404, json!({"error":2,"errorMessage":"Not found"}))
    );
    assert!(node.calls.lock().unwrap().is_empty());
}

#[tokio::test]
async fn invalid_address_precedes_node_and_body_validation() {
    let node = Arc::new(Node::default());
    assert_eq!(
        request_path(
            service(node.clone(), Arc::default(), 1000, 1),
            "invalid",
            "null"
        )
        .await
        .0,
        400
    );
    assert!(node.calls.lock().unwrap().is_empty());
}

#[tokio::test]
async fn unfinalized_recipient_precedes_body_validation_and_storage() {
    let node = Arc::new(Node {
        not_final: true,
        ..Node::default()
    });
    let store = Arc::new(Store::default());
    assert_eq!(
        request(service(node.clone(), store.clone(), 1000, 1), "null").await,
        (
            400,
            json!({"error":1,"errorMessage":"Account creation is not finalized"})
        )
    );
    assert_eq!(*node.calls.lock().unwrap(), ["account"]);
    assert!(store.calls.lock().unwrap().is_empty());
}

#[tokio::test]
async fn valid_json_with_invalid_schedule_is_not_a_normal_drop() {
    for body in ["null", "[]", "{}", "{\"type\":\"other\"}", "{\"type\":1}"] {
        let node = Arc::new(Node::default());
        assert_eq!(
            request(service(node.clone(), Arc::default(), 1000, 1), body).await,
            (
                400,
                json!({"error":1,"errorMessage":"Server configuration error"})
            )
        );
        assert_eq!(*node.calls.lock().unwrap(), ["account"]);
    }
}
