//! This module checks extended transaction envelopes and both authorization sets.
//! Source-defined bytes and hashes prevent sender or sponsor contents from being changed.

use super::*;

#[tokio::test]
async fn extended_submissions_keep_sender_sponsor_bytes_and_golden_identifiers() {
    for (sponsored, hash) in [
        (
            false,
            "734777b97794b0706a62676c9288953189f4f726d35c5b65f006b43054ecfae4",
        ),
        (
            true,
            "f5de8424f8e8629aef626a77cdfc87902c104a74b84301bb8c9e06fcd6bbbb85",
        ),
    ] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitExtended",
                fixtures::extended(sponsored).to_string()
            )
            .await,
            (200, json!({"submissionId":hash}))
        );
        assert_eq!(
            *node.items.lock().unwrap(),
            [fixtures::extended_bytes(sponsored)]
        );
    }
}

#[tokio::test]
async fn absent_null_and_empty_sponsor_maps_keep_the_legacy_zero_signature_sentinel() {
    let node = Arc::new(TestNode::default());
    for sponsor in [None, Some(Value::Null), Some(json!({}))] {
        let mut fixture = fixtures::extended(false);
        if let Some(sponsor) = sponsor {
            fixture["signatures"]["sponsor"] = sponsor;
        }
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitExtended",
                fixture.to_string()
            )
            .await,
            (
                200,
                json!({"submissionId":"734777b97794b0706a62676c9288953189f4f726d35c5b65f006b43054ecfae4"})
            )
        );
    }
    assert_eq!(
        *node.items.lock().unwrap(),
        vec![fixtures::extended_bytes(false); 3]
    );
}

#[tokio::test]
async fn sponsor_authorization_and_header_consistency_are_left_for_node_validation() {
    for header in [false, true] {
        for signatures in [false, true] {
            let node = Arc::new(TestNode::default());
            let mut fixture = fixtures::extended(signatures);
            fixture["transaction"] = fixtures::extended(header)["transaction"].clone();
            let mut expected = fixtures::extended_bytes(signatures);
            let original_body = fixtures::hex(
                fixtures::extended(signatures)["transaction"]
                    .as_str()
                    .unwrap(),
            );
            expected.truncate(expected.len() - original_body.len());
            expected.extend_from_slice(&fixtures::hex(fixture["transaction"].as_str().unwrap()));
            assert_eq!(
                request(
                    submission::routes(node.clone()),
                    "submitExtended",
                    fixture.to_string()
                )
                .await
                .0,
                200
            );
            assert_eq!(*node.items.lock().unwrap(), [expected]);
        }
    }
}

#[tokio::test]
async fn malformed_extended_signatures_headers_and_envelopes_never_submit() {
    let node = Arc::new(TestNode::default());
    let valid = fixtures::extended(true);
    let mut cases = vec![
        json!({}),
        json!({"signatures":{}}),
        json!({"signatures":{"sender":{}}}),
        json!({"signatures":{"sender":valid["signatures"]["sender"],"sponsor":"bad"},"transaction":valid["transaction"]}),
    ];
    for body in [
        "",
        "0",
        "zz",
        "0002",
        &valid["transaction"].as_str().unwrap()[..186],
    ] {
        let mut fixture = valid.clone();
        fixture["transaction"] = body.into();
        cases.push(fixture);
    }
    let mut bitmap = valid.clone();
    let mut binary = fixtures::hex(bitmap["transaction"].as_str().unwrap());
    binary[1] = 2;
    bitmap["transaction"] = fixtures::encoded(&binary).into();
    cases.push(bitmap);
    for fixture in cases {
        let (status, body) = request(
            submission::routes(node.clone()),
            "submitExtended",
            fixture.to_string(),
        )
        .await;
        assert_eq!(status, 400);
        assert!(body.get("submissionId").is_none());
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn extended_trailing_bytes_duplicate_fields_and_sponsor_indices_preserve_success_contents() {
    let node = Arc::new(TestNode::default());
    let fixture = fixtures::extended(true);
    let raw = format!(
        "{{\"signatures\":{},\"signatures\":{{}},\"transaction\":\"{}aabb\"}} junk",
        fixture["signatures"],
        fixture["transaction"].as_str().unwrap()
    );
    assert_eq!(
        request(submission::routes(node.clone()), "submitExtended", raw)
            .await
            .0,
        200
    );
    let aliased = fixture
        .to_string()
        .replace("\"2\":{\"7\":", "\"2e0\":{\"7.0\":");
    assert_eq!(
        request(submission::routes(node.clone()), "submitExtended", aliased)
            .await
            .0,
        200
    );
    assert_eq!(
        *node.items.lock().unwrap(),
        vec![fixtures::extended_bytes(true); 2]
    );
}
