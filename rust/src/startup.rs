//! This module prepares the resources required before the HTTP listener starts.
//! Startup stops if a catalogue, database connection, or required node query fails.

use crate::{
    config::{Config, load_catalogs},
    database::PostgresHistory,
    dependencies::Production,
    health::{Health, SystemClock},
    node::{Node, SdkNode},
};
use anyhow::Context;
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;

/// This type contains the resources acquired during startup.
pub struct Application {
    /// This service supplies the CCD account balance routes.
    pub balances: crate::balances::Balances<Production>,
    /// This service shares the node, database pool, and clock for health checks.
    pub health: Health<Production>,
    /// These JSON values contain catalogues in version order: 0, 1, then 2.
    pub catalogs: [serde_json::Value; 3],
    /// These settings supply wallet updates and terms.
    pub wallet: crate::wallet::WalletSettings,
    /// This optional service creates hosted Transak purchase sessions.
    pub transak: Option<Arc<crate::transak::Transak<SystemClock>>>,
    /// This service supplies optional testnet CCD drops.
    pub faucet: Arc<
        crate::faucet::Faucet<
            SdkNode,
            crate::faucet::database::PostgresDrops,
            crate::faucet::SystemDropTime,
        >,
    >,
    /// These node parameters remain available without another startup query.
    pub network: crate::network::Network<Production>,
}

/// Load catalogue data, connect to dependencies, and get cryptographic parameters.
/// Return an error before the HTTP listener starts if a required operation fails.
pub async fn initialize(config: &Config) -> anyhow::Result<Application> {
    config.validate()?;
    let catalogs = load_catalogs(&config.catalogs)?;
    let wallet = crate::wallet::WalletSettings::load(&config.wallet)?;
    let drop_settings = config
        .faucet
        .account
        .as_ref()
        .map(|path| crate::faucet::DropSettings::load(path, config.faucet.amount))
        .transpose()?;
    let transak = crate::transak::configured(config.transak.settings()?, SystemClock)?;
    let pool = PgPoolOptions::new()
        .max_connections(config.database.max_connections.get())
        .connect(&config.database.url)
        .await
        .context("Cannot initialize PostgreSQL pool")?;
    let drops = crate::faucet::database::PostgresDrops(pool.clone());
    if drop_settings.is_some() {
        drops
            .initialize()
            .await
            .context("Cannot initialize testnet drop recipient table")?;
    }
    let node = SdkNode::connect(&config.node).await?;
    let cryptographic_parameters = node.cryptographic_parameters().await?;
    Ok(Application {
        faucet: Arc::new(crate::faucet::Faucet {
            settings: drop_settings,
            node: Arc::new(node.clone()),
            store: Arc::new(drops),
            time: crate::faucet::SystemDropTime,
        }),
        balances: crate::balances::Balances { node: node.clone() },
        health: Health {
            node: node.clone(),
            history: Arc::new(PostgresHistory(pool)),
            clock: Arc::new(SystemClock),
            tolerance_seconds: config.server.health_tolerance_seconds,
        },
        catalogs,
        wallet,
        transak,
        network: crate::network::Network {
            node,
            parameters: Arc::new(cryptographic_parameters),
        },
    })
}
