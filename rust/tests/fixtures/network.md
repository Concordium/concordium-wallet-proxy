# Network fixture provenance

`network.json` is a source-derived HTTP expectation, written from the explicit
field list in `src/Proxy.hs:getConsensusInfoR`. The controlled protobuf input is
`support::consensus_info(false)`. It uses protocol 5, distinct hash bytes, a
height above JavaScript's exact-integer range, millisecond timestamps, and no
last-finalization time. The BFT expectation in `tests/network.rs` follows the
same handler's `bftObject` (protocol 6+, no slot duration).

Units and representations follow the Haskell types: `Duration` is an integer
number of milliseconds (`Concordium/Common/Time.hs`), heights/rounds/epochs are
integers, hashes are hex strings, and UTC times are ISO 8601 strings.
`lastFinalizedTime` is always a key and can be null; slot duration and BFT status
are omitted when absent. Node statistics must not leak into the HTTP response.
`getEpochLengthR` and `getGenesisHash` supply the two single-field expectations.

The global-context expectation follows `app/Main.hs` (version 0 wrapper),
`Concordium/Common/Version.hs` (`v` and `value`), and
`Concordium/ID/Parameters.hs` (Rust global-context JSON through FFI).
Tests independently construct the expected hex strings from binary field
encodings, rather than deriving expected JSON from the serializer under test.

Error expectations follow `runGRPCWithCustomError`, `ErrorCode`, and
`src/Internationalization/En.hs`. SDK conversion-error provenance and cookie
forwarding remain known compatibility blockers; the malformed-response test
records the SDK limitation rather than claiming Haskell parity.

Independent human review of these expectations remains outstanding. No fixture
was captured from replacement HTTP output or a public node.
