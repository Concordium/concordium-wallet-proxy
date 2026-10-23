//! This module constructs independent legacy bytes for submission fixtures.
//! It uses the Haskell field order and framing, not SDK transaction serialization.

use serde_json::{Value, json};

/// Read the existing initial or normal credential example without changing its expiry.
pub fn credential(normal: bool) -> Value {
    serde_json::from_str(if normal {
        include_str!("../../../examples/cdi.json")
    } else {
        include_str!("../../../examples/initial-account.json")
    })
    .unwrap()
}

/// Decode known hexadecimal fixture values independently of request decoding.
pub fn hex(text: &str) -> Vec<u8> {
    text.as_bytes()
        .as_chunks::<2>()
        .0
        .iter()
        .map(|bytes| {
            (char::from(bytes[0]).to_digit(16).unwrap() * 16
                + char::from(bytes[1]).to_digit(16).unwrap()) as u8
        })
        .collect()
}

/// Encode a byte sequence as lowercase hexadecimal for JSON test inputs.
pub fn encoded(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// Construct a standard transaction with distinct credential/key indices.
pub fn transaction() -> Value {
    let mut body = vec![1; 32];
    body.extend_from_slice(&u64::MAX.to_be_bytes());
    body.extend_from_slice(&45_000u64.to_be_bytes());
    body.extend_from_slice(&41u32.to_be_bytes());
    body.extend_from_slice(&u64::MAX.to_be_bytes());
    body.push(3);
    body.extend_from_slice(&[2; 32]);
    body.extend_from_slice(&999u64.to_be_bytes());
    json!({"signatures":{"0":{"0":"ab".repeat(64),"2":"cd".repeat(64)},"3":{"1":"ef".repeat(64)}}, "transaction":encoded(&body)})
}

/// Construct the expected serialized account block item from the source field layout.
pub fn transaction_bytes() -> Vec<u8> {
    let mut bytes = vec![0, 2, 0, 2, 0, 0, 64];
    bytes.extend_from_slice(&[0xab; 64]);
    bytes.extend_from_slice(&[2, 0, 64]);
    bytes.extend_from_slice(&[0xcd; 64]);
    bytes.extend_from_slice(&[3, 1, 1, 0, 64]);
    bytes.extend_from_slice(&[0xef; 64]);
    bytes.extend_from_slice(&hex(transaction()["transaction"].as_str().unwrap()));
    bytes
}

/// Construct the legacy extended JSON envelope, with or without a sponsor.
pub fn extended(sponsored: bool) -> Value {
    let standard = transaction();
    let body = hex(standard["transaction"].as_str().unwrap());
    let mut extended = u16::from(sponsored).to_be_bytes().to_vec();
    extended.extend_from_slice(&body[..60]);
    if sponsored {
        extended.extend_from_slice(&[4; 32]);
    }
    extended.extend_from_slice(&body[60..]);
    let mut signatures = json!({"sender":standard["signatures"]});
    if sponsored {
        signatures["sponsor"] = json!({"2":{"7":"12".repeat(64)}});
    }
    json!({"signatures":signatures,"transaction":encoded(&extended)})
}

/// Construct source-defined extended block-item bytes, including its tag.
pub fn extended_bytes(sponsored: bool) -> Vec<u8> {
    let standard = transaction_bytes();
    let body = hex(transaction()["transaction"].as_str().unwrap());
    let mut bytes = vec![3];
    bytes.extend_from_slice(&standard[1..standard.len() - body.len()]);
    if sponsored {
        bytes.extend_from_slice(&[1, 2, 1, 7, 0, 64]);
        bytes.extend_from_slice(&[0x12; 64]);
    } else {
        bytes.push(0);
    }
    bytes.extend_from_slice(&hex(extended(sponsored)["transaction"].as_str().unwrap()));
    bytes
}

/// Construct an update instruction around one source-defined payload.
pub fn update_bytes(payload: &[u8]) -> Vec<u8> {
    let mut bytes = vec![2];
    bytes.extend_from_slice(&5u64.to_be_bytes());
    bytes.extend_from_slice(&0u64.to_be_bytes());
    bytes.extend_from_slice(&u64::MAX.to_be_bytes());
    bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    bytes.extend_from_slice(payload);
    bytes.extend_from_slice(&[0, 1, 0, 7, 0, 64]);
    bytes.extend_from_slice(&[0x34; 64]);
    bytes
}

/// Construct the P10-compatible EUR-per-energy update payload.
pub fn exchange_rate_update() -> Vec<u8> {
    let mut payload = vec![3];
    payload.extend_from_slice(&2u64.to_be_bytes());
    payload.extend_from_slice(&3u64.to_be_bytes());
    update_bytes(&payload)
}

/// Construct the expected credential block item from the source field layout.
pub fn credential_bytes(normal: bool) -> Vec<u8> {
    let request = credential(normal);
    let credential = &request["value"]["credential"]["contents"];
    let mut bytes = vec![1];
    bytes.extend_from_slice(
        &request["value"]["messageExpiry"]
            .as_u64()
            .unwrap()
            .to_be_bytes(),
    );
    bytes.push(u8::from(normal));
    let keys = credential["credentialPublicKeys"]["keys"]
        .as_object()
        .unwrap();
    bytes.push(keys.len() as u8);
    for (index, key) in keys {
        bytes.extend_from_slice(&[index.parse::<u8>().unwrap(), 0]);
        bytes.extend_from_slice(&hex(key["verifyKey"].as_str().unwrap()));
    }
    bytes.push(
        credential["credentialPublicKeys"]["threshold"]
            .as_u64()
            .unwrap() as u8,
    );
    bytes.extend_from_slice(&hex(credential[if normal { "credId" } else { "regId" }]
        .as_str()
        .unwrap()));
    bytes.extend_from_slice(&(credential["ipIdentity"].as_u64().unwrap() as u32).to_be_bytes());
    if normal {
        bytes.push(credential["revocationThreshold"].as_u64().unwrap() as u8);
        let revokers = credential["arData"].as_object().unwrap();
        bytes.extend_from_slice(&(revokers.len() as u16).to_be_bytes());
        for (index, revoker) in revokers {
            bytes.extend_from_slice(&index.parse::<u32>().unwrap().to_be_bytes());
            bytes.extend_from_slice(&hex(revoker["encIdCredPubShare"].as_str().unwrap()));
        }
    }
    for name in ["validTo", "createdAt"] {
        let month = credential["policy"][name].as_str().unwrap();
        bytes.extend_from_slice(&month[..4].parse::<u16>().unwrap().to_be_bytes());
        bytes.push(month[4..].parse().unwrap());
    }
    if normal {
        // This fixture reveals only lastName, the attribute tag 1, with UTF-8 value "31".
        bytes.extend_from_slice(&[0, 1, 1, 2, b'3', b'1']);
        let proofs = hex(credential["proofs"].as_str().unwrap());
        bytes.extend_from_slice(&(proofs.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&proofs);
    } else {
        bytes.extend_from_slice(&[0, 0]);
        bytes.extend_from_slice(&hex(credential["sig"].as_str().unwrap()));
    }
    bytes
}
