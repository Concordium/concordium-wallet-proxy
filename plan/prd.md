# Wallet Proxy Replacement with Full Wallet Compatibility

## Problem Statement

Wallet users depend on the Wallet Proxy to access account balances, transaction history, transaction submission, identity-provider information, token information, and optional wallet services. Replacing the current service must not require wallet changes, disrupt existing integrations, lose historical information, or introduce new operational dependencies.

The current service has limited automated behavioral coverage. A replacement therefore needs explicit, independently reviewed evidence that existing wallet-facing behavior and operational integrations remain compatible, including historical interfaces and exceptional conditions.

Operators also need clearer, validated configuration and a controlled replacement process that preserves existing data and supports rollback without duplicating financial actions.

## Solution

Deliver a replacement Wallet Proxy that wallets can use without changing their existing integrations. Preserve the current service's observable behavior, historical interface versions, transaction-history access, and optional capabilities. Treat the current implementation as the behavioral authority when documentation and behavior differ.

Provide operators with startup-validated configuration and retain the existing node, transaction-history database, and optional on-ramp dependencies. Require no replacement history producer, database conversion, or additional production service.

Establish a comprehensive behavior-based acceptance catalogue using independently reviewed request and response fixtures, integration tests, and controlled failure scenarios. Deliver complete, tested capabilities incrementally, beginning with service startup and health reporting. Replace the deployed service only after the complete compatibility requirements are met.

## Use Cases

### UC 1: Check Account Balances

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet is connected to a supported network through the Wallet Proxy.
    * **Main Success Scenario (Flow):**
        1. The user opens an account.
        2. The wallet requests the balance using its existing interface version.
        3. The service returns the account information supported by that version.

**Acceptance Criteria (ACs):**

* **AC 1: Version-Appropriate Balance**
    * **Given** an account with available, staked, locked, encrypted, or cooldown balances
    * **When** the user views its balance
    * **Then** the returned amounts, staking details, cooldowns, and available balance match current behavior for the requested version.
* **AC 2: Finalization State**
    * **Given** an account that is absent or not yet finalized
    * **When** the user views its balance
    * **Then** current and finalized balance information is included or omitted exactly as in the current service.
* **AC 3: Protocol-Level Token Holdings**
    * **Given** an account holding protocol-level tokens
    * **When** the wallet requests a balance version supporting those tokens
    * **Then** balances, token information, and applicable allow-list and deny-list state match current behavior.

### UC 2: Obtain the Next Transaction Nonce

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user is preparing an account transaction.
    * **Main Success Scenario (Flow):**
        1. The user begins preparing a transaction.
        2. The wallet requests the next nonce.
        3. The service returns the next known nonce and its finalization reliability indication.

**Acceptance Criteria (ACs):**

* **AC 1: Pending Transactions**
    * **Given** finalized and potentially pending transactions for an account
    * **When** the user prepares another transaction
    * **Then** the nonce and reliability indication match current behavior.
* **AC 2: Invalid Account**
    * **Given** an invalid or unknown account
    * **When** its nonce is requested
    * **Then** the service preserves the current validation and error behavior.

### UC 3: Retrieve an Account Encryption Key

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet needs an account encryption key for an existing supported flow.
    * **Main Success Scenario (Flow):**
        1. The user starts that flow.
        2. The wallet requests the account encryption key.
        3. The service returns the key in the existing representation.

**Acceptance Criteria (ACs):**

* **AC 1: Successful Lookup**
    * **Given** an existing account
    * **When** its encryption key is requested
    * **Then** the returned key matches current behavior.
* **AC 2: Failed Lookup**
    * **Given** a malformed address or missing account
    * **When** its encryption key is requested
    * **Then** the existing distinction between those failures is preserved.

### UC 4: Estimate Transaction Fees

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user is preparing a currently supported transaction type.
    * **Main Success Scenario (Flow):**
        1. The user supplies transaction details.
        2. The wallet requests a fee estimate.
        3. The service returns the energy requirement and CCD cost, with an execution-success indication where currently provided.

**Acceptance Criteria (ACs):**

* **AC 1: Supported Transaction Types**
    * **Given** valid details for transfers, staking, delegation, smart-contract updates, or protocol-level token operations supported by the current service
    * **When** an estimate is requested
    * **Then** calculations, defaults, rounding, and returned information match current behavior.
* **AC 2: Invalid or Unsupported Inputs**
    * **Given** missing, malformed, boundary-value, or protocol-incompatible parameters
    * **When** an estimate is requested
    * **Then** validation order and error behavior match the current service.

### UC 5: Create an Account through Credential Deployment

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user has a credential deployment accepted by the existing interface.
    * **Main Success Scenario (Flow):**
        1. The user confirms account creation.
        2. The wallet submits the credential deployment.
        3. The service forwards it to the node and returns its submission identifier.

**Acceptance Criteria (ACs):**

* **AC 1: Accepted Deployment**
    * **Given** a supported credential deployment
    * **When** the user submits it
    * **Then** the submission and returned identifier match existing behavior.
* **AC 2: Unsuccessful Deployment**
    * **Given** malformed deployment data, node rejection, or node unavailability
    * **When** the user submits it
    * **Then** the service preserves the existing error contract and does not report successful submission incorrectly.

### UC 6: Submit an Account Transaction

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user has prepared and signed a transaction supported by the existing submission interface.
    * **Main Success Scenario (Flow):**
        1. The user confirms the transaction.
        2. The wallet submits it through its existing integration.
        3. The service forwards it to the node and returns the submission identifier.

**Acceptance Criteria (ACs):**

* **AC 1: Existing Submission Formats**
    * **Given** a valid transaction in an existing accepted submission format
    * **When** it is submitted
    * **Then** interpretation, submission, and the identifier match current behavior.
* **AC 2: Invalid Submission**
    * **Given** malformed data or a dependency failure
    * **When** submission is attempted
    * **Then** validation and errors match current behavior.

### UC 7: Submit a Sponsored Transaction

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user has a transaction with the required sender and sponsor authorization.
    * **Main Success Scenario (Flow):**
        1. The user confirms a sponsored transaction.
        2. The wallet submits the authorized transaction.
        3. The service submits it and returns its identifier.

**Acceptance Criteria (ACs):**

* **AC 1: Sponsorship Preserved**
    * **Given** a valid sponsored transaction
    * **When** it is submitted
    * **Then** sender and sponsor authorization and transaction contents are preserved.
* **AC 2: Invalid Authorization**
    * **Given** an invalid sponsored transaction
    * **When** submission is attempted
    * **Then** the response matches current behavior without introducing an alternative sponsorship model.

### UC 8: Submit a Raw Transaction

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet integrator
    * **Preconditions:** The integrator has a supported transaction in the existing raw binary format.
    * **Main Success Scenario (Flow):**
        1. The integrator submits the raw transaction.
        2. The service interprets and forwards it as currently supported.
        3. The integrator receives the existing submission response.

**Acceptance Criteria (ACs):**

* **AC 1: Binary Compatibility**
    * **Given** a valid raw transaction
    * **When** it is submitted
    * **Then** its meaning and submission identifier match existing behavior.
* **AC 2: Malformed Binary Input**
    * **Given** invalid or incomplete binary input
    * **When** it is submitted
    * **Then** rejection matches the existing HTTP and error contract.

### UC 9: Follow Submission Progress

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet has a transaction hash or submission identifier.
    * **Main Success Scenario (Flow):**
        1. The user views a submitted transaction.
        2. The wallet requests its status.
        3. The service returns its known state and available outcome details.

**Acceptance Criteria (ACs):**

* **AC 1: Transaction Lifecycle**
    * **Given** an absent, received, committed, or finalized submission
    * **When** the user checks its status
    * **Then** status and conditional response information match current behavior.
* **AC 2: Outcome Variants**
    * **Given** success, rejection, or differing outcomes across live blocks
    * **When** status is requested
    * **Then** outcome details, rejection descriptions, and block references match current behavior.

### UC 10: Browse Account Transaction History

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** Transaction history is available in the existing transaction-history database.
    * **Main Success Scenario (Flow):**
        1. The user opens account history.
        2. The wallet requests a page using its existing interface version.
        3. The service returns ordered transactions and pagination information.

**Acceptance Criteria (ACs):**

* **AC 1: Historical Versions**
    * **Given** transfers, memos, rewards, validator suspension events, or protocol-level token activity
    * **When** history is requested through any existing version
    * **Then** version-specific inclusion and representation match current behavior.
* **AC 2: Filtering and Pagination**
    * **Given** ordering, cursor, limit, time, height, reward, or encrypted-transaction filters
    * **When** history is requested
    * **Then** selection, defaults, boundaries, ordering, and pagination match current behavior.
* **AC 3: Empty and Invalid Requests**
    * **Given** an empty result or invalid filter
    * **When** history is requested
    * **Then** the current empty-response or error behavior is preserved.

### UC 11: Understand Transaction Effects

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet displays a transaction supported by the current service.
    * **Main Success Scenario (Flow):**
        1. The user opens transaction details.
        2. The wallet retrieves the transaction information.
        3. The service supplies descriptions, events, amounts, fees, and outcome information applicable to that transaction.

**Acceptance Criteria (ACs):**

* **AC 1: Complete Transaction Interpretation**
    * **Given** historical or currently supported account, credential, smart-contract, staking, reward, token, or lock-related activity
    * **When** its details are retrieved
    * **Then** descriptions and effects match current behavior.
* **AC 2: Sponsored and Self-Directed Transactions**
    * **Given** sponsorship or an account appearing in multiple transaction roles
    * **When** details are retrieved
    * **Then** fee attribution, balance changes, sponsor information, and conditional fields match current behavior.
* **AC 3: Rejection Details**
    * **Given** a rejected transaction
    * **When** the user requests its details
    * **Then** localized rejection information and optional raw rejection information match current behavior.

### UC 12: Obtain Identity-Provider Information

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** Identity-provider catalogues are configured.
    * **Main Success Scenario (Flow):**
        1. The user starts identity issuance or recovery.
        2. The wallet requests its supported catalogue version.
        3. The service returns provider information and applicable issuance and recovery metadata.

**Acceptance Criteria (ACs):**

* **AC 1: Catalogue Versions**
    * **Given** existing catalogue versions, including company identity providers where applicable
    * **When** the wallet requests a catalogue
    * **Then** provider inclusion, public information, and metadata match the configured current behavior.
* **AC 2: Existing Wallet Flows**
    * **Given** an older wallet using an older issuance flow
    * **When** it retrieves provider information
    * **Then** the replacement does not force migration to a newer flow.

### UC 13: Retrieve Cryptographic and Network Information

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet integrator
    * **Preconditions:** The service is connected to its configured network.
    * **Main Success Scenario (Flow):**
        1. The integrator requests cryptographic parameters, network identity, epoch length, or consensus information.
        2. The service supplies the requested information through the existing interface.
        3. The integrator uses it in an existing wallet flow.

**Acceptance Criteria (ACs):**

* **AC 1: Network Information Compatibility**
    * **Given** a configured network
    * **When** supported network information is requested
    * **Then** values, units, representations, and error behavior match the current service.
* **AC 2: Cryptographic Parameters**
    * **Given** successfully initialized cryptographic parameters
    * **When** they are requested
    * **Then** their contents and existing versioned representation remain compatible.

### UC 14: View Staking and Chain Conditions

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The user is reviewing staking or delegation choices.
    * **Main Success Scenario (Flow):**
        1. The user opens staking information.
        2. The wallet requests validator-pool information, passive delegation information, chain parameters, or the next payday.
        3. The service returns the available information.

**Acceptance Criteria (ACs):**

* **AC 1: Supported Protocol Conditions**
    * **Given** any protocol-dependent staking information currently supported
    * **When** the user views it
    * **Then** values and optional information match current behavior.
* **AC 2: Missing Pool or Dependency Failure**
    * **Given** an unknown pool or unavailable node
    * **When** information is requested
    * **Then** existing error behavior is preserved.

### UC 15: Discover Contract Tokens

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet supports the Concordium fungible and non-fungible token standard.
    * **Main Success Scenario (Flow):**
        1. The user views tokens for a contract.
        2. The wallet requests the known token list.
        3. The service returns token identifiers and supply information with existing pagination behavior.

**Acceptance Criteria (ACs):**

* **AC 1: Existing Token Catalogue**
    * **Given** token records in the existing database
    * **When** a contract token list is requested
    * **Then** identifiers, supply representation, ordering, and pagination match current behavior.
* **AC 2: Empty Catalogue**
    * **Given** no matching token records
    * **When** a list is requested
    * **Then** the existing empty-result behavior is preserved.

### UC 16: Retrieve Contract Token Metadata and Balances

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet has token identifiers and a contract address.
    * **Main Success Scenario (Flow):**
        1. The user opens token holdings.
        2. The wallet requests metadata references or account balances.
        3. The service queries the contract and returns the supported results.

**Acceptance Criteria (ACs):**

* **AC 1: Successful Queries**
    * **Given** valid token queries
    * **When** metadata references or balances are requested
    * **Then** results match current behavior.
* **AC 2: Partial Failure by Version**
    * **Given** a batch containing failing token queries
    * **When** either existing interface version is used
    * **Then** whole-request failure or omission of failed entries matches that version's current behavior.

### UC 17: Discover Protocol-Level Tokens

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet supports protocol-level tokens.
    * **Main Success Scenario (Flow):**
        1. The user browses available protocol-level tokens or opens a token.
        2. The wallet requests the token list or individual token information.
        3. The service returns token information and applicable module state and metadata references.

**Acceptance Criteria (ACs):**

* **AC 1: Token Information**
    * **Given** existing protocol-level tokens
    * **When** their information is requested
    * **Then** token identity, supply, decimals, module state, and metadata references match current behavior.
* **AC 2: Unknown Token**
    * **Given** an unknown or malformed token identifier
    * **When** token information is requested
    * **Then** the current validation and missing-token responses are preserved.

### UC 18: Find Accounts Associated with a Public Key

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet has a public key for account discovery.
    * **Main Success Scenario (Flow):**
        1. The user starts account discovery.
        2. The wallet requests accounts associated with the key, optionally restricting the account type.
        3. The service returns matching accounts.

**Acceptance Criteria (ACs):**

* **AC 1: Discovery Results**
    * **Given** existing public-key bindings
    * **When** accounts are requested
    * **Then** filtering, ordering, and results match current behavior.
* **AC 2: No Match or Invalid Key**
    * **Given** a key with no matches or malformed key data
    * **When** discovery is attempted
    * **Then** empty-result and validation behavior match the current service.

### UC 19: Request Testnet CCD

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Testnet wallet user
    * **Preconditions:** The optional CCD drop capability is enabled and funded.
    * **Main Success Scenario (Flow):**
        1. The user requests CCD for an account using an existing supported drop option.
        2. The service checks prior drop state and prepares or reuses the appropriate transaction.
        3. The service submits the transaction where appropriate and returns its identifier.

**Acceptance Criteria (ACs):**

* **AC 1: Existing Drop Options**
    * **Given** a valid request for a currently supported normal or scheduled drop
    * **When** the user requests CCD
    * **Then** amount, schedule rules, transaction handling, and response match current behavior.
* **AC 2: Retry and Persisted State**
    * **Given** an existing persisted drop transaction, including one created before replacement
    * **When** the user repeats the request
    * **Then** reuse, expiry, rejection, and retry behavior match the current service.
* **AC 3: Failure and Concurrency**
    * **Given** concurrent requests, unavailable dependencies, insufficient funds, or disabled drops
    * **When** a drop is requested
    * **Then** observable behavior and persistent effects conform to the reviewed compatibility cases.

### UC 20: Check Wallet Updates and Terms

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet supports the existing update and terms checks.
    * **Main Success Scenario (Flow):**
        1. The user opens the wallet.
        2. The wallet requests update guidance and applicable terms information.
        3. The service returns configured guidance and terms references.

**Acceptance Criteria (ACs):**

* **AC 1: Version and Platform Guidance**
    * **Given** configured forced and suggested update ranges for supported wallet generations and platforms
    * **When** a wallet checks its version
    * **Then** precedence, range boundaries, and destination links match current behavior.
* **AC 2: Optional Configuration**
    * **Given** absent update or terms configuration
    * **When** that information is requested
    * **Then** existing default or disabled-capability behavior is preserved.

### UC 21: Start a CCD Purchase through Transak

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The optional Transak integration is configured.
    * **Main Success Scenario (Flow):**
        1. The user chooses to buy CCD.
        2. The wallet requests a purchase session.
        3. The service obtains the session from Transak and returns the purchase destination.

**Acceptance Criteria (ACs):**

* **AC 1: Purchase Session**
    * **Given** a valid request
    * **When** a purchase session is requested
    * **Then** account information, configured environment, referrer, user-address source, and returned session match current behavior.
* **AC 2: Authentication and Failure**
    * **Given** expired access credentials, concurrent requests, or Transak failure
    * **When** users request sessions
    * **Then** refresh coordination, outbound requests, and returned errors match current behavior.
* **AC 3: Browser Access**
    * **Given** configured allowed browser origins
    * **When** a browser accesses the purchase capability
    * **Then** origin and preflight handling match current behavior.

### UC 22: Inspect Blocks and Block Transactions

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet integrator
    * **Preconditions:** The integrator needs chain information exposed by the current service.
    * **Main Success Scenario (Flow):**
        1. The integrator requests block information, blocks at a height, or transaction outcomes in a block.
        2. The service queries the configured node.
        3. The service returns the existing wallet-facing representation.

**Acceptance Criteria (ACs):**

* **AC 1: Block Queries**
    * **Given** valid block references or heights
    * **When** information is requested
    * **Then** block selection, transaction outcomes, and representations match current behavior.
* **AC 2: Invalid or Missing Block**
    * **Given** a malformed or unknown block reference
    * **When** it is queried
    * **Then** existing validation and missing-data behavior are preserved.

### UC 23: Receive Existing Localized Responses

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Wallet user
    * **Preconditions:** The wallet supplies language preferences through an existing supported mechanism.
    * **Main Success Scenario (Flow):**
        1. The user uses a wallet with a language preference.
        2. The wallet requests information or performs an action.
        3. The service returns applicable descriptions and errors using existing language selection rules.

**Acceptance Criteria (ACs):**

* **AC 1: Language Selection**
    * **Given** language preferences supplied through query, cookie, or request-header mechanisms
    * **When** a localized response is produced
    * **Then** precedence, supported language behavior, fallback, and wording match the current service.
* **AC 2: Error Consistency**
    * **Given** invalid input, missing data, an unavailable capability, or dependency failure
    * **When** the user requests an action
    * **Then** status, error classification, and message match the applicable existing behavior.

### UC 24: Configure and Start the Service

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Service operator
    * **Preconditions:** The operator has access to required dependency settings and catalogue data.
    * **Main Success Scenario (Flow):**
        1. The operator supplies configuration, environment overrides, and secret-file references as needed.
        2. The operator starts the service.
        3. The service validates configuration and performs required initialization before accepting requests.

**Acceptance Criteria (ACs):**

* **AC 1: Operational Capabilities**
    * **Given** valid configuration
    * **When** the service starts
    * **Then** existing connection security, node targeting, database access, catalogue versions, and configured optional capabilities remain available.
* **AC 2: Invalid Startup State**
    * **Given** invalid configuration or a failed required initialization dependency
    * **When** startup is attempted
    * **Then** the service fails with an actionable diagnostic rather than serving with an incomplete required state.
* **AC 3: Configuration Lifecycle**
    * **Given** a running service
    * **When** an operator changes configuration
    * **Then** a restart is required to apply the change.

### UC 25: Check Service Health

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Service operator
    * **Preconditions:** The service is running.
    * **Main Success Scenario (Flow):**
        1. The operator or their monitoring system requests service health.
        2. The service checks the node and transaction-history database as currently required.
        3. The service reports health using existing freshness and response rules.

**Acceptance Criteria (ACs):**

* **AC 1: Healthy and Stale Dependencies**
    * **Given** accessible dependencies and chain data that is fresh or stale relative to the configured tolerance
    * **When** health is requested
    * **Then** the reported result matches current behavior.
* **AC 2: Dependency Failure**
    * **Given** a failed node or database check
    * **When** health is requested
    * **Then** the existing failure response is preserved.

### UC 26: Review Compatibility Evidence before Release

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Release maintainer
    * **Preconditions:** A capability or release candidate is ready for review.
    * **Main Success Scenario (Flow):**
        1. The maintainer reviews the behavior coverage catalogue and independently derived fixtures.
        2. The maintainer reviews automated test results and unresolved compatibility issues.
        3. The maintainer approves release only when all required compatibility gates pass.

**Acceptance Criteria (ACs):**

* **AC 1: Independent Expectations**
    * **Given** request and response fixtures derived from current behavior and documented examples
    * **When** the maintainer reviews them
    * **Then** expected results are traceable to their source and are not justified solely by replacement-service output.
* **AC 2: Release Blockers**
    * **Given** missing capabilities, dependency-support gaps, unexplained differences, or failed required tests
    * **When** release is considered
    * **Then** release remains blocked unless the user explicitly approves the applicable exception.

### UC 27: Replace or Roll Back the Deployed Service

* **Priority:** Must-Have
* **Use Case:**
    * **Primary Actor:** Service operator
    * **Preconditions:** Both implementations can be run independently, and the replacement has passed acceptance.
    * **Main Success Scenario (Flow):**
        1. The operator switches service deployment to the replacement.
        2. Wallets continue using their existing integrations and data.
        3. If needed, the operator switches back using interoperable existing state.

**Acceptance Criteria (ACs):**

* **AC 1: Data-Preserving Replacement**
    * **Given** existing transaction history and persisted faucet state
    * **When** the operator replaces or rolls back the service
    * **Then** neither a database conversion nor a replacement transaction-history producer is required.
* **AC 2: Exclusive Faucet Operation**
    * **Given** the same faucet funding account
    * **When** deployment changes or rollback occurs
    * **Then** only one implementation actively serves faucet requests at a time.
    * **And** state-changing requests are not shadowed into production.

## Non-Functional Requirements

### NFR 1: Complete Observable Compatibility

* **Priority:** Must-Have
* **Requirement:** All existing wallet-facing interfaces and historical versions retain current observable behavior unless a specific exception is explicitly approved. Current implementation behavior takes precedence over conflicting documentation.

**Acceptance Criteria (ACs):**

* **AC 1: Contract Coverage**
    * **Given** the complete inventory of existing capabilities
    * **When** compatibility is assessed
    * **Then** coverage includes methods, paths, query interpretation, validation order, status codes, relevant headers, redirects, browser-origin behavior, localization, binary inputs, and response bodies.
* **AC 2: Representation Compatibility**
    * **Given** a response with numeric values, timestamps, optional values, or collections
    * **When** it is compared with the approved expectation
    * **Then** units, precision, ordering where meaningful, and omitted-versus-null distinctions match current behavior.
    * **And** JSON whitespace and object-key order need not match.

### NFR 2: Existing Runtime and Data Interoperability

* **Priority:** Must-Have
* **Requirement:** The replacement uses the existing Concordium node interface, PostgreSQL transaction-history database, and optional Transak integration without adding required production services or changing externally owned data structures.

**Acceptance Criteria (ACs):**

* **AC 1: Existing Data**
    * **Given** historical transaction records, token records, account-key bindings, and serialized faucet transactions
    * **When** the replacement accesses them
    * **Then** it interprets them compatibly without a data migration.
* **AC 2: Ownership Boundary**
    * **Given** an existing transaction-history database
    * **When** the replacement initializes
    * **Then** it does not migrate the transaction-history producer's data structures.
    * **And** its own data management remains limited to the existing faucet-recipient capability.

### NFR 3: Runtime Policy Preservation

* **Priority:** Must-Have
* **Requirement:** Existing externally meaningful startup, timeout, retry, caching, optional-capability, and health policies remain compatible.

**Acceptance Criteria (ACs):**

* **AC 1: Failure Policies**
    * **Given** dependency failure, stale data, or missing optional configuration
    * **When** the relevant policy applies
    * **Then** behavior follows the existing service rather than introducing unapproved retries, fallback data, or caching semantics.

### NFR 4: Behavior-Based Test Coverage

* **Priority:** Must-Have
* **Requirement:** Every existing route and version has deterministic behavioral acceptance coverage, including important success, failure, and boundary scenarios. Line coverage alone is not acceptance evidence.

**Acceptance Criteria (ACs):**

* **AC 1: Fixture Completeness**
    * **Given** a supported interface
    * **When** its tests are reviewed
    * **Then** independently reviewed request and response fixtures include relevant HTTP metadata and binary inputs where needed.
* **AC 2: Controlled Nondeterminism**
    * **Given** time-dependent or randomized behavior
    * **When** it is tested
    * **Then** controlled inputs or explicit reviewed invariants establish correctness without hiding entire response fields.
* **AC 3: Independent Oracle**
    * **Given** fixture-based compatibility testing
    * **When** expected outputs are established
    * **Then** they are derived from current behavior and reviewed examples, not automatically blessed from replacement output.
    * **And** tests do not execute the current implementation as a comparison service.

### NFR 5: Integration and Side-Effect Coverage

* **Priority:** Must-Have
* **Requirement:** Tests establish compatibility of dependency interactions and persistent effects, not just response bodies.

**Acceptance Criteria (ACs):**

* **AC 1: Dependencies and State**
    * **Given** node, database, or on-ramp interactions
    * **When** tests exercise a capability
    * **Then** they verify relevant outbound operations, database effects, failures, retries, and concurrent behavior using controlled dependencies and real PostgreSQL integration tests.
* **AC 2: Reproducible Data Environment**
    * **Given** a clean test environment
    * **When** database tests are prepared
    * **Then** versioned test-only data structures and synthetic records reproduce relevant legacy types, constraints, and indexes without production data.

### NFR 6: Automated Quality and Release Gates

* **Priority:** Must-Have
* **Requirement:** Deterministic unit, HTTP, and database integration checks, formatting, static analysis, and database-query validation run for every proposed change. Controlled real-node end-to-end acceptance is required before release.

**Acceptance Criteria (ACs):**

* **AC 1: Change Validation**
    * **Given** a proposed change
    * **When** automated validation runs
    * **Then** required deterministic checks run without depending on a public node.
* **AC 2: Release Validation**
    * **Given** a release candidate
    * **When** acceptance is assessed
    * **Then** the complete fixture-based compatibility suite and controlled real-node tests pass.

### NFR 7: Compile-Time Database Query Validation

* **Priority:** Must-Have
* **Requirement:** Database queries are checked against the existing database structure at build time by default, with reproducible validation supporting builds without a live database. Unchecked exceptions require explicit approval.

**Acceptance Criteria (ACs):**

* **AC 1: Query Validation**
    * **Given** a database query or database-structure fixture change
    * **When** automated checks run
    * **Then** stored query-validation information is verified against the representative database structure.
* **AC 2: Filter Compatibility**
    * **Given** transaction-history filtering and ordering combinations
    * **When** checked queries execute
    * **Then** they preserve the existing selection and pagination contract.

### NFR 8: Financial and Operational Test Safety

* **Priority:** Must-Have
* **Requirement:** Compatibility testing must not replay transactions, faucet requests, or purchase-session creation into production. Deployment must not permit simultaneous active faucet implementations for the same funding account.

**Acceptance Criteria (ACs):**

* **AC 1: Safe Test Effects**
    * **Given** tests for state-changing operations
    * **When** they execute
    * **Then** their effects are confined to controlled test dependencies and environments.
* **AC 2: Cutover Safety**
    * **Given** deployment replacement or rollback
    * **When** faucet traffic is enabled
    * **Then** exclusivity of the active implementation is enforced operationally.

### NFR 9: Incremental Delivery without Partial Production Replacement

* **Priority:** Must-Have
* **Requirement:** Delivery proceeds through complete, tested capabilities, starting with startup, configuration, and health. Compatibility risks are resolved within each capability rather than through a separate preliminary spike.

**Acceptance Criteria (ACs):**

* **AC 1: Capability Completion**
    * **Given** an increment with an unresolved dependency-support or legacy-format issue
    * **When** it is reviewed
    * **Then** it remains incomplete until resolved or explicitly excepted.
* **AC 2: Production Readiness**
    * **Given** an incomplete replacement
    * **When** deployment is considered
    * **Then** it does not receive production traffic as a partial replacement.

### NFR 10: No Silent Compatibility Workarounds

* **Priority:** Must-Have
* **Requirement:** Missing dependency support must not result in silently omitted functionality, changed behavior, a privately maintained dependency fork, or a bypass of agreed dependency boundaries.

**Acceptance Criteria (ACs):**

* **AC 1: Blocking Gap**
    * **Given** a capability that cannot be delivered with the selected supported dependency behavior
    * **When** the gap is discovered
    * **Then** it is surfaced for an explicit decision and blocks that capability and release as applicable.
    * **And** upstream support, including a pinned supported revision when necessary, is preferred.

## Out of Scope

- Changing wallet user experiences or requiring existing wallet integrations to migrate.
- Removing historical interface versions, correcting existing quirks, or changing error wording without explicit approval.
- New wallet-facing features beyond the current service's capabilities.
- Replacing the Concordium node, transaction-history producer, or transaction-history database design.
- Migrating existing transaction-history or faucet data into a new format.
- Introducing microservices, a new production routing gateway, or additional required runtime services.
- Implementing an alternative sponsorship protocol or changing chain-level transaction rules.
- Preserving old configuration names, command-line conventions, or startup scripts as compatibility requirements.
- Configuration hot reload.
- Direct automated comparisons against a running current implementation.
- A separate preliminary compatibility spike.
- Performance benchmarking or comparative performance evidence as release acceptance requirements.
- A mandatory line-coverage percentage as a substitute for behavioral coverage.
- Production shadowing of state-changing requests or simultaneous active faucet implementations using the same funding account.
- New public operational endpoints or a required external telemetry service.
