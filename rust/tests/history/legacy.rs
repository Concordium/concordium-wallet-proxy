//! These tests cover historical summary categories and strict legacy field decoding.
//! Input crypto values come from source-defined fixtures; expected descriptions are independent literals.

use super::http::*;
use crate::blocks::fixtures;
use concordium_wallet_proxy::history;
use serde_json::{Value, json};

#[tokio::test]
async fn every_pre_token_account_type_keeps_its_legacy_response_name_and_description() {
    for (kind, rendered, description) in [
        ("deployModule", "deployModule", "Deploy module"),
        ("initContract", "initContract", "Initialize smart contract"),
        ("update", "update", "Invoke smart contract"),
        ("transfer", "transfer", "Transfer"),
        ("transferWithMemo", "transfer", "Transfer with a memo"),
        (
            "updateBakerStake",
            "updateBakerStake",
            "Update validator stake",
        ),
        (
            "updateBakerKeys",
            "updateBakerKeys",
            "Update validator keys",
        ),
        (
            "updateBakerRestakeEarnings",
            "updateBakerRestakeEarnings",
            "Update whether to restake validator earnings",
        ),
        ("addBaker", "addBaker", "Add validator"),
        ("removeBaker", "removeBaker", "Remove validator"),
        (
            "updateCredentialKeys",
            "updateAccountKeys",
            "Update credential keys",
        ),
        (
            "encryptedAmountTransfer",
            "encryptedAmountTransfer",
            "Shielded transfer",
        ),
        (
            "encryptedAmountTransferWithMemo",
            "encryptedAmountTransfer",
            "Shielded transfer with a memo",
        ),
        (
            "transferToEncrypted",
            "transferToEncrypted",
            "Shielded amount",
        ),
        ("transferToPublic", "transferToPublic", "Unshielded amount"),
        (
            "transferWithSchedule",
            "transferWithSchedule",
            "Transfer with schedule",
        ),
        (
            "transferWithScheduleAndMemo",
            "transferWithSchedule",
            "Transfer with schedule and a memo",
        ),
        (
            "updateCredentials",
            "updateCredentials",
            "Update account credentials",
        ),
        ("registerData", "registerData", "Register data on the chain"),
        ("configureBaker", "configureBaker", "Configure validator"),
        (
            "configureDelegation",
            "configureDelegation",
            "Configure delegation",
        ),
    ] {
        let body = entry(summary(kind, vec![]), 0, SENDER, "").await;
        assert_eq!(
            body["details"],
            json!({"type":rendered,"description":description,"outcome":"success","events":[]})
        );
    }
}

#[tokio::test]
async fn credential_and_chain_update_categories_decode_without_an_account_sender() {
    let mut value = summary("transfer", vec![]);
    value["Left"]["sender"] = Value::Null;
    value["Left"]["cost"] = json!("0");
    for (kind, description) in [
        ("initial", "Deploy initial account credential"),
        ("normal", "Deploy account credential"),
    ] {
        value["Left"]["type"] = json!({"type":"credentialDeploymentTransaction","contents":kind});
        let body = entry(value.clone(), 1, SENDER, "").await;
        assert_eq!(body["details"]["type"], "deployCredential");
        assert_eq!(body["details"]["description"], description);
        assert_eq!(body["origin"], json!({"type":"none"}));
        assert!(body.get("cost").is_none());
    }
    for (kind, description) in [
        ("updateProtocol", "Protocol update"),
        ("updateElectionDifficulty", "Update election difficulty"),
        (
            "updateEuroPerEnergy",
            "Update Euro per Energy exchange rate",
        ),
        (
            "updateMicroGTUPerEuro",
            "Update micro CCD per Euro exchange rate",
        ),
        (
            "updateFoundationAccount",
            "Update the foundation account address",
        ),
        (
            "updateMintDistribution",
            "Update parameters of the mint distribution",
        ),
        (
            "updateTransactionFeeDistribution",
            "Update transaction fee distribution",
        ),
        (
            "updateGASRewards",
            "Update parameters for GAS rewards distribution",
        ),
        ("updatePoolParameters", "Update staking pool parameters"),
        ("updateTimeParameters", "Update time parameters"),
        ("updateCooldownParameters", "Update cooldown parameters"),
        ("updateAddAnonymityRevoker", "Add anonymity revoker"),
        ("updateAddIdentityProvider", "Add identity provider"),
        ("updateRootKeys", "Update root keys"),
        ("updateLevel1Keys", "Update level 1 keys"),
        ("updateLevel2Keys", "Update level 2 keys"),
        ("updateTimeoutParameters", "Update timeout parameters"),
        ("updateMinBlockTime", "Update minimum block time"),
        ("updateBlockEnergyLimit", "Update block energy limit"),
        (
            "updateFinalizationCommitteeParameters",
            "Update finalization committee parameters",
        ),
        (
            "updateValidatorScoreParameters",
            "Update validator score parameters",
        ),
    ] {
        value["Left"]["type"] = json!({"type":"updateTransaction","contents":kind});
        let body = entry(value.clone(), 2, SENDER, "").await;
        assert_eq!(body["details"]["type"], "chainUpdate");
        assert_eq!(body["details"]["description"], description);
    }
}

#[tokio::test]
async fn module_credential_validator_key_and_governance_events_decode_legacy_records() {
    let credential = fixtures::credential().to_string();
    let keys = serde_json::to_value(fixtures::baker_keys()).unwrap();
    let mut added = keys.clone();
    added["tag"] = json!("BakerAdded");
    added["stake"] = json!("1000000");
    added["restakeEarnings"] = json!(true);
    let mut updated = keys;
    updated["tag"] = json!("BakerKeysUpdated");
    for (event, text) in [
        (
            json!({"tag":"ModuleDeployed","contents":"05".repeat(32)}),
            format!("Deployed module {}", "05".repeat(32)),
        ),
        (
            json!({"tag":"AccountCreated","contents":SENDER}),
            format!("Created account with address {SENDER}"),
        ),
        (
            json!({"tag":"CredentialDeployed","regId":credential,"account":SENDER}),
            format!("Deployed a credential to account {SENDER}"),
        ),
        (
            json!({"tag":"CredentialKeysUpdated","credId":credential}),
            format!("Updated keys of credential with ID {credential}"),
        ),
        (
            json!({"tag":"CredentialsUpdated","account":SENDER,"newCredIds":[credential],"removedCredIds":[],"newThreshold":1}),
            format!("Credentials on account {SENDER} updated."),
        ),
        (
            added,
            format!("Added validator {SENDER} (ID 42) and initial stake 1.000000 CCD"),
        ),
        (updated, format!("Validator {SENDER} (ID 42) keys updated.")),
        (
            json!({"tag":"UpdateEnqueued","effectiveTime":1,"payload":{"updateType":"foundationAccount","update":SENDER}}),
            "Chain update event enqueued.".into(),
        ),
    ] {
        let body = entry(summary("transfer", vec![event]), 2, SENDER, "").await;
        assert_eq!(body["details"]["events"], json!([text]));
    }
}

#[tokio::test]
async fn malformed_required_fields_and_future_variants_fail_the_entire_page() {
    for event in [
        json!({"tag":"Transferred","from":{"type":"AddressAccount","address":SENDER},"to":{"type":"AddressAccount","address":RECEIVER}}),
        json!({"tag":"BakerAdded","bakerId":1,"account":SENDER,"stake":"1","restakeEarnings":true}),
        json!({"tag":"ContractInitialized","ref":"05".repeat(32),"address":{"index":1,"subindex":0},"amount":"0","initName":"init_test","events":[]}),
        json!({"tag":"ContractInitialized","ref":"05".repeat(32),"address":{"index":1,"subindex":0},"amount":"0","initName":"init_test","events":[],"parameter":"","contractVersion":null}),
        json!({"tag":"NewEncryptedAmount","account":SENDER,"newIndex":1,"encryptedAmount":"ff"}),
        json!({"tag":"Resumed","address":{"index":1,"subindex":0},"success":"yes"}),
        json!({"tag":"CredentialsUpdated","account":SENDER,"newCredIds":[],"removedCredIds":[],"newThreshold":0}),
        json!({"tag":"TokenCreated","payload":{}}),
    ] {
        let (node, database) = adapters(vec![
            summary("transfer", vec![]),
            summary("transfer", vec![event]),
        ]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v2/accTransactions/{SENDER}")
            )
            .await,
            (502, json!({"error":0,"errorMessage":"Database error"}))
        );
    }
    let (node, database) = adapters(vec![summary("unknownTransaction", vec![])]);
    assert_eq!(
        request(
            history::routes(node, database),
            &format!("/v1/accTransactions/{SENDER}")
        )
        .await
        .0,
        502
    );
}
