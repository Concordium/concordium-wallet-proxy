//! This module checks raw block-item tags, protocol boundaries, and binary acceptance.
//! It compares complete submitted bytes with independent source-defined fixtures.

use super::*;

#[tokio::test]
async fn raw_submission_supports_every_legacy_block_item_kind_and_golden_identifier() {
    for (bytes, hash) in [
        (
            fixtures::transaction_bytes(),
            "cf836d6b87d0bcb141d85e73b1b33814871253843044509fd14b2ea4478e6768",
        ),
        (
            fixtures::credential_bytes(false),
            "67fde09681c15241f40a045f31afe5659318d5f87044d15240b682d48de0851a",
        ),
        (
            fixtures::credential_bytes(true),
            "af8d639191fa6194c0ff285bcedb1f47b8b05ca477aae68dae26e1c524e06888",
        ),
        (
            fixtures::extended_bytes(false),
            "734777b97794b0706a62676c9288953189f4f726d35c5b65f006b43054ecfae4",
        ),
        (
            fixtures::extended_bytes(true),
            "f5de8424f8e8629aef626a77cdfc87902c104a74b84301bb8c9e06fcd6bbbb85",
        ),
        (
            fixtures::exchange_rate_update(),
            "99cfc039ad665a2ac2ee8fe2645b09b80c2eb2735769106e7ebc26618adc23a1",
        ),
    ] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                bytes.clone()
            )
            .await,
            (200, json!({"submissionId":hash}))
        );
        assert_eq!(*node.items.lock().unwrap(), [bytes]);
    }
}

#[tokio::test]
async fn raw_unknown_truncated_noncanonical_and_inaccurate_encodings_never_submit() {
    let node = Arc::new(TestNode::default());
    let fixtures = [
        fixtures::transaction_bytes(),
        fixtures::credential_bytes(false),
        fixtures::credential_bytes(true),
        fixtures::extended_bytes(true),
        fixtures::exchange_rate_update(),
    ];
    let mut cases = vec![vec![], vec![4], vec![255], vec![b'{', b'}']];
    for fixture in &fixtures {
        for length in [1, 2, 8, 30, fixture.len() - 1] {
            cases.push(fixture[..length].to_vec());
        }
    }
    let mut unordered = fixtures::transaction_bytes();
    unordered[2] = 3;
    unordered[138] = 0;
    cases.push(unordered);
    let mut empty = fixtures::transaction_bytes();
    empty[1] = 0;
    cases.push(empty);
    let mut bitmap = fixtures::extended_bytes(true);
    let header = bitmap.len()
        - fixtures::hex(fixtures::extended(true)["transaction"].as_str().unwrap()).len();
    bitmap[header + 1] = 2;
    cases.push(bitmap);
    let mut huge = fixtures::exchange_rate_update();
    huge[25..29].copy_from_slice(&u32::MAX.to_be_bytes());
    cases.push(huge);
    let mut unsigned = fixtures::exchange_rate_update();
    let signature = unsigned.len() - 70;
    unsigned[signature..signature + 2].copy_from_slice(&[0, 0]);
    cases.push(unsigned);
    for bytes in cases {
        let (status, body) = request(
            submission::routes(node.clone()),
            "submitRawTransaction",
            bytes,
        )
        .await;
        assert_eq!(status, 400);
        assert!(body.get("submissionId").is_none());
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn raw_governance_respects_hardcoded_p10_tags_and_isolated_payload_lengths() {
    let node = Arc::new(TestNode::default());
    for tag in [0u8, 2, 6, 8, 9, 25, 255] {
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                fixtures::update_bytes(&[tag; 100])
            )
            .await
            .0,
            400
        );
    }
    for (tag, subtag) in [(10, 2), (10, 3), (10, 5), (11, 1), (11, 2), (11, 4)] {
        let mut payload = vec![tag, subtag];
        payload.extend_from_slice(&[0; 1000]);
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                fixtures::update_bytes(&payload)
            )
            .await
            .0,
            400
        );
    }
    let update = fixtures::exchange_rate_update();
    let payload = &update[29..46];
    for bytes in [
        payload[..payload.len() - 1].to_vec(),
        [payload, &[0]].concat(),
    ] {
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                fixtures::update_bytes(&bytes)
            )
            .await
            .0,
            400
        );
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn raw_p10_higher_level_key_updates_preserve_valid_payloads() {
    for (tag, subtag) in [(10, 0), (10, 1), (11, 0)] {
        let mut payload = vec![tag, subtag, 0, 1, 0];
        payload.extend_from_slice(&fixtures::hex(
            "af9980c8595ebdb732f81b24483a4b5432f381aafc1832659998798d000349f2",
        ));
        payload.extend_from_slice(&[0, 1]);
        let expected = fixtures::update_bytes(&payload);
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                expected.clone()
            )
            .await
            .0,
            200
        );
        assert_eq!(*node.items.lock().unwrap(), [expected]);
    }
}

#[tokio::test]
async fn sdk_supported_old_and_p11_governance_variants_are_still_rejected_by_p10() {
    use concordium_rust_sdk::{base::updates::UpdatePayload, common::Deserial};
    let node = Arc::new(TestNode::default());
    let mut election = vec![2];
    election.extend_from_slice(&50_000u32.to_be_bytes());
    let mut locks = vec![25];
    locks.extend_from_slice(&3_600_000u64.to_be_bytes());
    for payload in [election, locks] {
        assert!(UpdatePayload::deserial(&mut std::io::Cursor::new(&payload)).is_ok());
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                fixtures::update_bytes(&payload)
            )
            .await
            .0,
            400
        );
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn raw_outer_trailing_bytes_are_ignored_for_every_item_kind() {
    for bytes in [
        fixtures::transaction_bytes(),
        fixtures::credential_bytes(false),
        fixtures::credential_bytes(true),
        fixtures::extended_bytes(true),
        fixtures::exchange_rate_update(),
    ] {
        let node = Arc::new(TestNode::default());
        let mut trailing = bytes.clone();
        trailing.extend_from_slice(&[0xab, 0xcd]);
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                trailing
            )
            .await
            .0,
            200
        );
        assert_eq!(*node.items.lock().unwrap(), [bytes]);
    }
}

#[tokio::test]
async fn extended_and_raw_node_failures_keep_transaction_rejection_and_no_identifier() {
    let node = Arc::new(TestNode {
        reject: true,
        ..Default::default()
    });
    for (route, body) in [
        (
            "submitExtended",
            fixtures::extended(true).to_string().into_bytes(),
        ),
        ("submitRawTransaction", fixtures::credential_bytes(false)),
    ] {
        assert_eq!(
            request(submission::routes(node.clone()), route, body).await,
            (
                400,
                json!({"error":1,"errorMessage":"Transaction rejected by the node"})
            )
        );
    }
    assert_eq!(node.items.lock().unwrap().len(), 2);
}
