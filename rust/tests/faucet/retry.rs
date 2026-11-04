//! This module checks persisted drop reuse, replacement, and dependency failures.
//! No test submits transactions outside the controlled node adapter.

use super::*;

fn persisted() -> Arc<Store> {
    Arc::new(Store {
        row: Mutex::new(Some(Record {
            id: 42,
            transaction: include_bytes!("../fixtures/faucet/normal.bin").to_vec(),
        })),
        next_id: AtomicU64::new(43),
        ..Store::default()
    })
}

#[tokio::test]
async fn pending_or_successful_transaction_is_reused_without_balance_or_expiry_rejection() {
    let node = Arc::new(Node {
        balance: 0,
        recipient_nonce: 100,
        ..Node::default()
    });
    let store = persisted();
    let transaction = saved(&store);
    assert_eq!(
        request(
            service(node.clone(), store.clone(), 2000, 15),
            "{\"type\":\"scheduled\"}"
        )
        .await,
        (
            200,
            json!({"submissionId":Submission::AccountTransaction(transaction).hash()})
        )
    );
    assert_eq!(
        *node.calls.lock().unwrap(),
        ["account", "account", "status"]
    );
    assert!(node.submissions.lock().unwrap().is_empty());
    assert_eq!(store.row.lock().unwrap().as_ref().unwrap().id, 42);
}

#[tokio::test]
async fn absent_or_rejected_transaction_is_resent_at_expiry_equality() {
    for now in [1000, 1300] {
        let node = Arc::new(Node {
            acceptable: false,
            recipient_nonce: 10,
            ..Node::default()
        });
        let store = persisted();
        assert_eq!(
            request(service(node.clone(), store.clone(), now, 1), "")
                .await
                .0,
            200
        );
        assert_eq!(
            *node.submissions.lock().unwrap(),
            [include_bytes!("../fixtures/faucet/normal.bin").to_vec()]
        );
        assert_eq!(store.row.lock().unwrap().as_ref().unwrap().id, 42);
    }
}

#[tokio::test]
async fn insufficient_recipient_balance_precedes_expiry_and_nonce_replacement() {
    let node = Arc::new(Node {
        acceptable: false,
        balance: 2_001_000_000,
        recipient_nonce: 99,
        ..Node::default()
    });
    let store = persisted();
    assert_eq!(
        request(service(node.clone(), store.clone(), 2000, 1), "").await,
        (
            502,
            json!({"error":0,"errorMessage":"Server configuration error"})
        )
    );
    assert!(node.submissions.lock().unwrap().is_empty());
    assert_eq!(*store.calls.lock().unwrap(), ["lookup"]);
}

#[tokio::test]
async fn expired_or_nonce_invalid_transaction_is_deleted_and_replaced() {
    for (now, recipient_nonce) in [(1301, 10), (1000, 11)] {
        let node = Arc::new(Node {
            acceptable: false,
            balance: 2_001_000_001,
            recipient_nonce,
            ..Node::default()
        });
        let store = persisted();
        assert_eq!(
            request(
                service(node.clone(), store.clone(), now, 4),
                "{\"type\":\"scheduled\"}"
            )
            .await
            .0,
            200
        );
        assert_eq!(
            *store.calls.lock().unwrap(),
            ["lookup", "delete", "lookup", "insert"]
        );
        assert_eq!(
            *node.calls.lock().unwrap(),
            ["account", "account", "status", "nonce", "submit"]
        );
        assert_eq!(store.row.lock().unwrap().as_ref().unwrap().id, 43);
        assert_eq!(saved(&store).header.expiry.seconds, now + 300);
    }
}

#[tokio::test]
async fn submission_failure_keeps_signed_bytes_for_the_next_request() {
    let node = Arc::new(Node {
        fail: Some("submit"),
        ..Node::default()
    });
    let store = Arc::new(Store::default());
    assert_eq!(
        request(service(node, store.clone(), 1000, 4), "").await,
        (
            400,
            json!({"error":1,"errorMessage":"Server configuration error"})
        )
    );
    assert_eq!(
        store.row.lock().unwrap().as_ref().unwrap().transaction,
        include_bytes!("../fixtures/faucet/normal.bin").as_slice()
    );
    let node = Arc::new(Node {
        acceptable: false,
        ..Node::default()
    });
    assert_eq!(
        request(
            service(node.clone(), store, 1001, 15),
            "{\"type\":\"scheduled\"}"
        )
        .await
        .0,
        200
    );
    assert_eq!(
        *node.submissions.lock().unwrap(),
        [include_bytes!("../fixtures/faucet/normal.bin").to_vec()]
    );
}

#[tokio::test]
async fn dependency_failures_preserve_rows_and_do_not_submit() {
    for call in ["account", "status", "nonce"] {
        let node = Arc::new(Node {
            fail: Some(call),
            ..Node::default()
        });
        let store = if call == "nonce" {
            Arc::default()
        } else {
            persisted()
        };
        assert_eq!(
            request(service(node.clone(), store, 1000, 1), "").await.0,
            502
        );
        assert!(node.submissions.lock().unwrap().is_empty());
    }
    let node = Arc::new(Node::default());
    let store = Arc::new(Store {
        fail: true,
        ..Store::default()
    });
    assert_eq!(
        request(service(node.clone(), store, 1000, 1), "").await.0,
        500
    );
    assert!(node.submissions.lock().unwrap().is_empty());
}

#[tokio::test]
async fn corrupt_persisted_transaction_is_not_deleted_or_submitted() {
    for bytes in [
        vec![0],
        [
            include_bytes!("../fixtures/faucet/normal.bin").as_slice(),
            &[0],
        ]
        .concat(),
    ] {
        let node = Arc::new(Node::default());
        let store = Arc::new(Store {
            row: Mutex::new(Some(Record {
                id: 42,
                transaction: bytes,
            })),
            ..Store::default()
        });
        assert_eq!(
            request(service(node.clone(), store.clone(), 1000, 1), "")
                .await
                .0,
            500
        );
        assert!(node.submissions.lock().unwrap().is_empty());
        assert_eq!(store.row.lock().unwrap().as_ref().unwrap().id, 42);
    }
}

#[tokio::test]
async fn insert_conflict_discards_new_signature_and_reuses_the_winning_record() {
    let node = Arc::new(Node::default());
    let store = Arc::new(Store {
        conflict: true,
        ..Store::default()
    });
    let transaction: Transaction = from_bytes(&mut std::io::Cursor::new(include_bytes!(
        "../fixtures/faucet/scheduled.bin"
    )))
    .unwrap();
    assert_eq!(
        request(service(node.clone(), store.clone(), 1000, 1), "").await,
        (
            200,
            json!({"submissionId":Submission::AccountTransaction(transaction).hash()})
        )
    );
    assert!(node.submissions.lock().unwrap().is_empty());
    assert_eq!(
        *node.calls.lock().unwrap(),
        ["account", "nonce", "account", "status"]
    );
    assert_eq!(*store.calls.lock().unwrap(), ["lookup", "insert", "lookup"]);
}

#[tokio::test]
async fn concurrent_requests_keep_a_single_recipient_record_and_identifier() {
    let node = Arc::new(Node::default());
    let store = Arc::new(Store::default());
    let app = service(node.clone(), store.clone(), 1000, 4);
    let (first, second) = tokio::join!(
        request(app.clone(), ""),
        request(app, "{\"type\":\"scheduled\"}")
    );
    assert_eq!(first, second);
    assert_eq!(first.0, 200);
    assert_eq!(store.next_id.load(Ordering::SeqCst), 1);
}
