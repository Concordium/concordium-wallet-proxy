# Changelog for wallet-proxy

## Unreleased changes

 - Disable sessions since we don't use them.
 - Add query parameter `includeRawRejectReason` to the `accTransactions` query.
 - Health query checks connections to database and GRPC, and that the last final
   block is less than `health-tolerance` seconds old. `health-tolerance` is 300
   seconds unless an alternative value is chosen at startup.
 - Add filtering options `blockTimeFrom`, `blockTimeTo`, `blockRewards`, 
  `finalizationRewards`, `bakingRewards`, and  `onlyEncrypted` to the 
  `accTransactions` query.

## 0.5.0
 - Make the GTU drop functionality optional.
