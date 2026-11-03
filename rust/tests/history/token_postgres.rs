//! This test serves mixed legacy token/CCD/lock rows through real HTTP, PostgreSQL, and SDK lookup.
//! The persisted fixtures use the external schema without a migration or production effects.

use super::{
    http::*,
    rpc,
    token_events::{lock_fixture, token_transfer},
};
use crate::{health, support};
use concordium_rust_sdk::id::types::AccountAddress;
use concordium_wallet_proxy::{database::PostgresHistory, history};
use serde_json::{Value, json};
use sqlx::PgPool;
use std::sync::Arc;

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn v3_history_mixes_legacy_tokens_locks_rejections_and_ccd_without_changing_old_versions(
    pool: PgPool,
) {
    super::schema(&pool).await;
    let mut token = token_transfer();
    token["memo"] = json!("00ff");
    let mut sponsored = summary("tokenUpdate", vec![token]);
    sponsored["Left"]["sponsorDetails"] = json!({"sponsor":SENDER,"cost":"7"});
    let mut creation = summary(
        "transfer",
        vec![
            json!({"tag":"TokenCreated","payload":{"tokenId":"USD","tokenModule":"05".repeat(32),"decimals":6,"initializationParameters":"a0"}}),
        ],
    );
    creation["Left"]["type"] = json!({"type":"updateTransaction","contents":"updateCreatePLT"});
    creation["Left"]["sender"] = Value::Null;
    let reason = json!({"tag":"LockReturnNotAuthorized","contents":{"lockId":{"accountIndex":10,"sequenceNumber":11,"creationOrder":12},"account":SENDER}});
    let mut rejection = summary("metaUpdate", vec![]);
    rejection["Left"]["result"] = json!({"outcome":"reject","rejectReason":reason});
    for (index,fixture) in [
        summary("transferWithMemo",vec![transfer(SENDER,RECEIVER,"1"),json!({"tag":"TransferMemo","memo":"aa"})]),
        sponsored,lock_fixture("created"),lock_fixture("transfer"),creation,rejection,
        json!({"Right":{"tag":"PaydayAccountReward","account":SENDER,"transactionFees":"1","bakerReward":"2","finalizationReward":"3"}}),
        summary("tokenUpdate",vec![json!({"tag":"TokenModuleEvent","tokenId":"USD","type":"pause","details":"a0"})]),
        summary("tokenUpdate",vec![json!({"tag":"TokenBurn","tokenId":"USD","target":{"type":"account","address":SENDER},"amount":{"value":"1","decimals":0}})]),
        lock_fixture("destroyed"),
    ].into_iter().enumerate() { super::insert(&pool,index as i64 + 1,AccountAddress([1;32]),fixture).await; }
    let (_rpc, node) = rpc::sdk(Arc::new(rpc::RpcState::default())).await;
    let routes = history::routes(node, Arc::new(PostgresHistory(pool.clone())));
    let (app, _) = health::app_with_routes("missing", false, true, routes);
    let server = support::serve_router(app).await;
    let client = reqwest::Client::builder()
        .no_proxy()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();
    // The supplied receiver alias resolves to the SDK fixture's canonical sender.
    let base = format!("{}/v3/accTransactions/{RECEIVER}", server.endpoint);
    let response = client
        .get(format!("{base}?includeRawRejectReason=n&lang=da"))
        .header("accept-language", "fr")
        .header("cookie", "_LANG=zh")
        .header("origin", "https://wallet.example")
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["content-language"], "en");
    assert_eq!(
        response.headers()["content-type"],
        "application/json; charset=utf-8"
    );
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
    let body = response.json::<Value>().await.unwrap();
    assert_eq!(body["count"], 10);
    assert_eq!(
        body["transactions"]
            .as_array()
            .unwrap()
            .iter()
            .map(|row| row["id"].as_i64().unwrap())
            .collect::<Vec<_>>(),
        (1..=10).collect::<Vec<_>>()
    );
    assert_eq!(body["transactions"][0]["subtotal"], "-1");
    assert_eq!(body["transactions"][0]["details"]["memo"], "aa");
    assert_eq!(
        body["transactions"][1]["details"]["tokenTransferAmount"],
        json!({"value":"10","decimals":2})
    );
    assert_eq!(body["transactions"][1]["cost"], "7");
    assert_eq!(body["transactions"][1]["total"], "-7");
    assert!(body["transactions"][1].get("subtotal").is_none());
    assert_eq!(
        body["transactions"][2]["details"]["lockConfig"],
        "a1647465737401"
    );
    assert_eq!(
        body["transactions"][3]["details"]["fromLock"]["accountIndex"],
        30
    );
    assert_eq!(
        body["transactions"][4]["details"]["description"],
        "Update create new PLT token"
    );
    assert_eq!(
        body["transactions"][5]["details"]["rawRejectReason"],
        reason
    );
    assert_eq!(
        body["transactions"][5]["details"]["rejectReason"],
        format!("Account {SENDER} is not authorized to return from lock WV6MDZDimp.")
    );
    assert_eq!(body["transactions"][6]["total"], "6");
    assert_eq!(
        body["transactions"][9]["details"]["lockId"]["accountIndex"],
        20
    );
    for version in 0..3 {
        let body = client
            .get(format!(
                "{}/v{version}/accTransactions/{RECEIVER}",
                server.endpoint
            ))
            .send()
            .await
            .unwrap()
            .json::<Value>()
            .await
            .unwrap();
        assert_eq!(body["count"], 2);
        assert_eq!(body["transactions"][0]["id"], 1);
        assert_eq!(body["transactions"][1]["id"], 7);
        assert_eq!(
            body["transactions"][0]["details"].get("memo").is_some(),
            version > 0
        );
    }
    for (query, ids) in [
        ("order=descending&from=7&limit=2", vec![6, 5]),
        (
            "blockTimeFrom=2&blockTimeTo=6&blockHeightFrom=3&blockHeightTo=5",
            vec![3, 4, 5],
        ),
        ("includeRewards=none", vec![1, 2, 3, 4, 5, 6, 8, 9, 10]),
        ("onlyEncrypted=y", vec![]),
        ("limit=0", vec![]),
    ] {
        let body = client
            .get(format!("{base}?{query}"))
            .send()
            .await
            .unwrap()
            .json::<Value>()
            .await
            .unwrap();
        assert_eq!(
            body["transactions"]
                .as_array()
                .unwrap()
                .iter()
                .map(|row| row["id"].as_i64().unwrap())
                .collect::<Vec<_>>(),
            ids
        );
    }
    let response = client.head(&base).send().await.unwrap();
    assert_eq!(response.status(), 200);
    assert!(response.bytes().await.unwrap().is_empty());
    assert_eq!(client.post(&base).send().await.unwrap().status(), 405);
    assert_eq!(
        client
            .get(format!("{base}/"))
            .send()
            .await
            .unwrap()
            .status(),
        301
    );
    super::insert(
        &pool,
        11,
        AccountAddress([1; 32]),
        summary("metaUpdate", vec![json!({"tag":"FutureLockEvent"})]),
    )
    .await;
    assert_eq!(
        client
            .get(format!("{base}?limit=10"))
            .send()
            .await
            .unwrap()
            .status(),
        200
    );
    let response = client.get(&base).send().await.unwrap();
    assert_eq!(response.status(), 502);
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        json!({"error":0,"errorMessage":"Database error"})
    );
    sqlx::raw_sql("DROP TABLE ati")
        .execute(&pool)
        .await
        .unwrap();
    assert_eq!(client.get(&base).send().await.unwrap().status(), 500);
}
