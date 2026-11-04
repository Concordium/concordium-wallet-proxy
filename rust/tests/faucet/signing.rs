//! This module checks signed drop bytes, energy, expiry, and release boundaries.
//! Signature verification uses public access structures rather than replacement JSON.

use super::*;
use concordium_rust_sdk::types::transactions::{AccountAccessStructure, Payload};

#[tokio::test]
async fn normal_empty_and_malformed_bodies_create_signed_transfers() {
    for body in ["", "{", "{\"type\":\"normal\"}"] {
        let node = Arc::new(Node::default());
        let store = Arc::new(Store::default());
        let app = service(node.clone(), store.clone(), 1000, 4);
        let (status, value) = request(app.clone(), body).await;
        let transaction = saved(&store);
        let item = Submission::AccountTransaction(transaction.clone());
        assert_eq!((status, value), (200, json!({"submissionId":item.hash()})));
        assert_eq!(transaction.header.sender, SENDER);
        assert_eq!(transaction.header.nonce, Nonce::from(10));
        assert_eq!(transaction.header.expiry.seconds, 1300);
        assert_eq!(transaction.header.energy_amount.energy, 501);
        assert_eq!(u32::from(transaction.header.payload_size), 41);
        assert!(
            transaction.verify_transaction_signature(&AccountAccessStructure::from(
                &app.settings.as_ref().unwrap().keys
            ))
        );
        assert!(
            matches!(transaction.payload.decode().unwrap(), Payload::Transfer { to_address, amount } if to_address == RECIPIENT && amount.micro_ccd == 2_000_000_001)
        );
        assert_eq!(*node.submissions.lock().unwrap(), [encode(&transaction)]);
        assert_eq!(*node.calls.lock().unwrap(), ["account", "nonce", "submit"]);
        assert_eq!(*store.calls.lock().unwrap(), ["lookup", "insert"]);
    }
}

#[tokio::test]
async fn schedules_preserve_legacy_delayed_first_release_and_remainder() {
    for count in [1, 4, 15] {
        let store = Arc::new(Store::default());
        let app = service(Arc::default(), store.clone(), 1000, count);
        assert_eq!(
            request(app.clone(), "{\"type\":\"scheduled\"}").await.0,
            200
        );
        let transaction = saved(&store);
        assert_eq!(transaction.header.energy_amount.energy, 194 + 380 * count);
        assert_eq!(transaction.header.expiry.seconds, 1300);
        assert!(
            transaction.verify_transaction_signature(&AccountAccessStructure::from(
                &app.settings.as_ref().unwrap().keys
            ))
        );
        let Payload::TransferWithSchedule { to, schedule } = transaction.payload.decode().unwrap()
        else {
            panic!("Expected scheduled transfer");
        };
        assert_eq!(to, RECIPIENT);
        assert_eq!(schedule.len(), count as usize);
        assert_eq!(
            schedule
                .iter()
                .map(|(_, amount)| amount.micro_ccd)
                .sum::<u64>(),
            2_000_000_001
        );
        for (index, (timestamp, amount)) in schedule.iter().enumerate() {
            assert_eq!(
                timestamp.timestamp_millis(),
                1_000_000 + (index as u64 + 2) * 300_000
            );
            assert_eq!(
                amount.micro_ccd,
                2_000_000_001 / count + if index == 0 { 2_000_000_001 % count } else { 0 }
            );
        }
    }
}

#[tokio::test]
async fn signing_selects_ascending_threshold_keys_and_counts_actual_signatures() {
    let base = serde_json::to_value(&settings().keys).unwrap();
    let key = base["keys"]["0"]["keys"]["0"].clone();
    for (keys, prefix) in [
        (
            json!({"threshold":1,"keys":{"5":{"threshold":1,"keys":{"0":key}},"1":{"threshold":1,"keys":{"9":key,"2":key}}}}),
            vec![1, 1, 1, 2, 0, 64],
        ),
        (
            json!({"threshold":2,"keys":{"0":{"threshold":2,"keys":{"0":key}}}}),
            vec![1, 0, 1, 0, 0, 64],
        ),
    ] {
        let mut settings = settings();
        settings.keys = serde_json::from_value(keys).unwrap();
        let store = Arc::new(Store::default());
        let app = Arc::new(Faucet {
            settings: Some(settings),
            node: Arc::new(Node::default()),
            store: store.clone(),
            time: Time {
                now: 1000,
                count: 4,
            },
        });
        assert_eq!(request(app, "").await.0, 200);
        let transaction = saved(&store);
        assert_eq!(transaction.header.energy_amount.energy, 501);
        assert!(encode(&transaction).starts_with(&prefix));
    }
}

#[tokio::test]
async fn schedule_parser_uses_first_duplicate_key_and_ignores_trailing_input() {
    for body in [
        "{\"type\":\"scheduled\"}trailing",
        "{\"type\":\"scheduled\",\"type\":\"normal\"}",
    ] {
        let store = Arc::new(Store::default());
        assert_eq!(
            request(service(Arc::default(), store.clone(), 1000, 4), body)
                .await
                .0,
            200
        );
        assert_eq!(
            store.row.lock().unwrap().as_ref().unwrap().transaction,
            include_bytes!("../fixtures/faucet/scheduled.bin").as_slice()
        );
    }
}

#[test]
fn drop_key_loading_errors_do_not_disclose_secret_contents() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("keys.json");
    let bytes =
        serde_json::to_vec(&json!({"address":SENDER,"accountKeys":settings().keys})).unwrap();
    std::fs::write(&path, bytes).unwrap();
    let settings = DropSettings::load(&path, Amount::from_micro_ccd(9)).unwrap();
    assert_eq!(settings.address, SENDER);
    assert_eq!(settings.amount.micro_ccd, 9);
    for secret in ["not-json-secret", "{\"accountKeys\":\"secret\"}"] {
        std::fs::write(&path, secret).unwrap();
        let error = DropSettings::load(&path, Amount::zero())
            .err()
            .unwrap()
            .to_string();
        assert_eq!(error, "Invalid drop account key file");
        assert!(!error.contains(secret));
    }
}

#[tokio::test]
async fn independent_legacy_binary_fixtures_round_trip_and_match_new_signatures() {
    for (body, fixture) in [
        (
            "normal",
            include_bytes!("../fixtures/faucet/normal.bin").as_slice(),
        ),
        (
            "scheduled",
            include_bytes!("../fixtures/faucet/scheduled.bin").as_slice(),
        ),
    ] {
        let node = Arc::new(Node::default());
        let store = Arc::new(Store::default());
        assert_eq!(
            request(
                service(node, store.clone(), 1000, 4),
                &format!("{{\"type\":\"{body}\"}}")
            )
            .await
            .0,
            200
        );
        assert_eq!(
            store.row.lock().unwrap().as_ref().unwrap().transaction,
            fixture
        );
        let decoded: Transaction = from_bytes(&mut std::io::Cursor::new(fixture)).unwrap();
        assert_eq!(encode(&decoded), fixture);
    }
}
