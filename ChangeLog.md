# Changelog for wallet-proxy

## Unreleased changes

 - Disable sessions since we don't use them.
 - Add query parameter `includeRawRejectReason` to the `accTransactions` query.
 - health query checks connections to database and GRPC, and that the last final
   block is less than 5 minutes old.

## 0.5.0
 - Make the GTU drop functionality optional.
