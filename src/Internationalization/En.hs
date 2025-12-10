{-# LANGUAGE OverloadedStrings #-}

module Internationalization.En where

import Concordium.Client.Config (showPrettyJSON)
import Concordium.Client.Utils
import Concordium.ID.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Wasm (contractAndFunctionName, initContractName)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Internationalization.Base
import Yesod

translation :: I18n
translation = I18n{..}
  where
    i18nMalformedTransaction = "Malformed transaction body"
    i18nUpdateTransaction UpdateProtocol = "Protocol update"
    i18nUpdateTransaction UpdateElectionDifficulty = "Update election difficulty"
    i18nUpdateTransaction UpdateEuroPerEnergy = "Update Euro per Energy exchange rate"
    i18nUpdateTransaction UpdateMicroGTUPerEuro = "Update micro CCD per Euro exchange rate"
    i18nUpdateTransaction UpdateFoundationAccount = "Update the foundation account address"
    i18nUpdateTransaction UpdateMintDistribution = "Update parameters of the mint distribution"
    i18nUpdateTransaction UpdateTransactionFeeDistribution = "Update transaction fee distribution"
    i18nUpdateTransaction UpdateGASRewards = "Update parameters for GAS rewards distribution"
    i18nUpdateTransaction UpdatePoolParameters = "Update staking pool parameters"
    i18nUpdateTransaction UpdateTimeParameters = "Update time parameters"
    i18nUpdateTransaction UpdateCooldownParameters = "Update cooldown parameters"
    i18nUpdateTransaction UpdateAddAnonymityRevoker = "Add anonymity revoker"
    i18nUpdateTransaction UpdateAddIdentityProvider = "Add identity provider"
    i18nUpdateTransaction UpdateRootKeys = "Update root keys"
    i18nUpdateTransaction UpdateLevel1Keys = "Update level 1 keys"
    i18nUpdateTransaction UpdateLevel2Keys = "Update level 2 keys"
    i18nUpdateTransaction UpdateTimeoutParameters = "Update timeout parameters"
    i18nUpdateTransaction UpdateMinBlockTime = "Update minimum block time"
    i18nUpdateTransaction UpdateBlockEnergyLimit = "Update block energy limit"
    i18nUpdateTransaction UpdateFinalizationCommitteeParameters = "Update finalization committee parameters"
    i18nUpdateTransaction UpdateValidatorScoreParameters = "Update validator score parameters"
    i18nUpdateTransaction UpdateCreatePLT = "Update create new PLT token"

    i18nRejectReason ModuleNotWF = "Typechecking of module failed"
    i18nRejectReason (ModuleHashAlreadyExists mref) = "A module with the hash " <> descrModule mref <> " already exists"
    i18nRejectReason (InvalidAccountReference addr) = "The account " <> descrAccount addr <> " does not exist"
    i18nRejectReason (InvalidModuleReference mref) = "Module does not exist: " <> descrModule mref
    i18nRejectReason (InvalidContractAddress caddr) = "No smart contract instance exists with address " <> descrInstance caddr
    i18nRejectReason (AmountTooLarge addr _) = "The sending account (" <> descrAddress addr <> ") has insufficient funds"
    i18nRejectReason SerializationFailure = "Malformed transaction body"
    i18nRejectReason OutOfEnergy = "Insufficient energy"
    i18nRejectReason RejectedInit{..} = "Failed contract initialization due to contract logic with error code " <> Text.pack (show rejectReason)
    i18nRejectReason RejectedReceive{..} = "Failed contract receive invocation due to contract logic with reason " <> Text.pack (show rejectReason)
    i18nRejectReason InvalidProof = "Invalid proof"
    i18nRejectReason (InvalidInitMethod mref initName) = "Init method " <> descrInitName initName <> " does not exist in module " <> descrModule mref <> "."
    i18nRejectReason (InvalidReceiveMethod mref receiveName) = "Receive method " <> descrReceiveName receiveName <> " does not exist for module " <> descrModule mref <> "."
    i18nRejectReason RuntimeFailure = "Runtime failure when executing smart contract."
    i18nRejectReason (DuplicateAggregationKey _) = "Duplicate aggregation key."
    i18nRejectReason KeyIndexAlreadyInUse = "The requested key index is already in use."
    i18nRejectReason InvalidCredentialKeySignThreshold = "The requested threshold would exceed the number of keys on the credential."
    i18nRejectReason InvalidAccountThreshold = "The requested account threshold would exceed the number of credentials."
    i18nRejectReason InvalidEncryptedAmountTransferProof = "The encrypted amount transfer has an invalid proof."
    i18nRejectReason (EncryptedAmountSelfTransfer _) = "An encrypted amount transfer from the account to itself is not allowed."
    i18nRejectReason InvalidTransferToPublicProof = "The secret to public transfer has an invalid proof."
    i18nRejectReason InvalidIndexOnEncryptedTransfer = "The provided encryped amount index is out of bounds."
    i18nRejectReason ZeroScheduledAmount = "Attempt to transfer 0 CCD with schedule."
    i18nRejectReason NonIncreasingSchedule = "Attempt to transfer amount with non-increasing schedule."
    i18nRejectReason FirstScheduledReleaseExpired = "The first scheduled release is in the past."
    i18nRejectReason (ScheduledSelfTransfer _) = "Attempt to transfer from account A to A with schedule."
    i18nRejectReason (AlreadyABaker bid) = "Validator with ID " <> Text.pack (show bid) <> " already exists."
    i18nRejectReason (NotABaker addr) = "Account " <> descrAccount addr <> " is not a validator."
    i18nRejectReason InsufficientBalanceForBakerStake = "Sender account has insufficient balance to cover the requested stake."
    i18nRejectReason BakerInCooldown = "Request to make change to the validator while the validator is in the cooldown period."
    i18nRejectReason NonExistentCredentialID = "Credential ID does not exist."
    i18nRejectReason InvalidCredentials = "One or more of the credentials is invalid."
    i18nRejectReason (DuplicateCredIDs _) = "One or more of the credential IDs is duplicate."
    i18nRejectReason (NonExistentCredIDs _) = "One or more of the credential IDs does not exist."
    i18nRejectReason RemoveFirstCredential = "Attempt to remove the first credential."
    i18nRejectReason CredentialHolderDidNotSign = "Credential holder did not sign the key update."
    i18nRejectReason StakeUnderMinimumThresholdForBaking = "Desired stake is below the minimum threshold."
    i18nRejectReason NotAllowedMultipleCredentials = "The account is not allowed to have multiple credentials."
    i18nRejectReason NotAllowedToReceiveEncrypted = "The account is not allowed to receive encrypted transfers."
    i18nRejectReason NotAllowedToHandleEncrypted = "The account is not allowed to handle encrypted amounts."
    i18nRejectReason MissingBakerAddParameters = "One or more arguments are missing in order to add a validator."
    i18nRejectReason FinalizationRewardCommissionNotInRange = "Finalization reward commission rate is not within the allowed range."
    i18nRejectReason BakingRewardCommissionNotInRange = "Block reward commission rate is not within the allowed range."
    i18nRejectReason TransactionFeeCommissionNotInRange = "Transaction fee commission rate is not within the allowed range."
    i18nRejectReason AlreadyADelegator = "Attempt to add validator that is already a validator."
    i18nRejectReason InsufficientBalanceForDelegationStake = "Sender account has insufficient balance to cover the requested stake."
    i18nRejectReason MissingDelegationAddParameters = "One or more arguments are missing in order to add a delegator."
    i18nRejectReason DelegatorInCooldown = "Request to make change to the validator while the validator is in the cooldown period."
    i18nRejectReason (NotADelegator addr) = "Account " <> descrAccount addr <> " is not a delegator."
    i18nRejectReason (DelegationTargetNotABaker _) = "Delegation target is not a validator."
    i18nRejectReason StakeOverMaximumThresholdForPool = "Stake is above maximum threshold."
    i18nRejectReason PoolWouldBecomeOverDelegated = "Delegation stake is too high, over-delegation is not allowed."
    i18nRejectReason PoolClosed = "Delegation to a closed pool is not allowed."
    i18nRejectReason InsufficientDelegationStake = "Adding a delegator with 0 stake is not allowed."
    i18nRejectReason (NonExistentTokenId tokenId) = "Non existing plt token id " <> Text.pack (show tokenId) <> "."
    i18nRejectReason (TokenUpdateTransactionFailed reason) = "Token update transaction failed with reason: " <> Text.pack (show reason) <> "."

    i18nTransactionType TTDeployModule = "Deploy module"
    i18nTransactionType TTInitContract = "Initialize smart contract"
    i18nTransactionType TTUpdate = "Invoke smart contract"
    i18nTransactionType TTTransfer = "Transfer"
    i18nTransactionType TTTransferWithMemo = "Transfer with a memo"
    i18nTransactionType TTUpdateBakerStake = "Update validator stake"
    i18nTransactionType TTUpdateBakerKeys = "Update validator keys"
    i18nTransactionType TTUpdateBakerRestakeEarnings = "Update whether to restake validator earnings"
    i18nTransactionType TTAddBaker = "Add validator"
    i18nTransactionType TTRemoveBaker = "Remove validator"
    i18nTransactionType TTUpdateCredentialKeys = "Update credential keys"
    i18nTransactionType TTEncryptedAmountTransfer = "Shielded transfer"
    i18nTransactionType TTEncryptedAmountTransferWithMemo = "Shielded transfer with a memo"
    i18nTransactionType TTTransferToEncrypted = "Shielded amount"
    i18nTransactionType TTTransferToPublic = "Unshielded amount"
    i18nTransactionType TTTransferWithSchedule = "Transfer with schedule"
    i18nTransactionType TTTransferWithScheduleAndMemo = "Transfer with schedule and a memo"
    i18nTransactionType TTUpdateCredentials = "Update account credentials"
    i18nTransactionType TTRegisterData = "Register data on the chain"
    i18nTransactionType TTConfigureBaker = "Configure validator"
    i18nTransactionType TTConfigureDelegation = "Configure delegation"
    i18nTransactionType TTTokenUpdate = "Token update"

    i18nDeployCredential Initial = "Deploy initial account credential"
    i18nDeployCredential Normal = "Deploy account credential"

    i18nSupplementedEvent (ModuleDeployed mref) = "Deployed module " <> descrModule mref
    i18nSupplementedEvent ContractInitialized{..} =
        "Initialized smart contract "
            <> initContractName ecInitName
            <> " at address "
            <> descrInstance ecAddress
            <> " with balance "
            <> descrAmount ecAmount
            <> ". "
            <> Text.pack (show (length ecEvents))
            <> " events were logged"
    i18nSupplementedEvent Updated{..} =
        let (cname, fname) = contractAndFunctionName euReceiveName
        in  "Invoked smart contract: source="
                <> descrAddress euInstigator
                <> ", target="
                <> descrInstance euAddress
                <> ", contract= "
                <> cname
                <> ", function= "
                <> fname
                <> ", amount="
                <> descrAmount euAmount
                <> ", message="
                <> Text.pack (show euMessage)
    i18nSupplementedEvent (Transferred sender amt recv) = "Transferred " <> descrAmount amt <> " from " <> descrAddress sender <> " to " <> descrAddress recv
    i18nSupplementedEvent (AccountCreated addr) = "Created account with address " <> descrAccount addr
    i18nSupplementedEvent (CredentialDeployed _ addr) = "Deployed a credential to account " <> descrAccount addr
    i18nSupplementedEvent BakerAdded{..} = "Added validator " <> descrBaker ebaBakerId ebaAccount <> " and initial stake " <> descrAmount ebaStake
    i18nSupplementedEvent BakerRemoved{..} = "Removed validator " <> descrBaker ebrBakerId ebrAccount
    i18nSupplementedEvent BakerStakeIncreased{..} = "Stake of validator " <> descrBaker ebsiBakerId ebsiAccount <> " increased to " <> descrAmount ebsiNewStake
    i18nSupplementedEvent BakerStakeDecreased{..} = "Stake of validator " <> descrBaker ebsiBakerId ebsiAccount <> " decreased to " <> descrAmount ebsiNewStake
    i18nSupplementedEvent BakerSetRestakeEarnings{..} = "Validator " <> descrBaker ebsreBakerId ebsreAccount <> if ebsreRestakeEarnings then " set to restake earnings." else " unset restaking of earnings."
    i18nSupplementedEvent BakerKeysUpdated{..} = "Validator " <> descrBaker ebkuBakerId ebkuAccount <> " keys updated."
    i18nSupplementedEvent CredentialKeysUpdated{..} = "Updated keys of credential with ID " <> Text.pack (show ckuCredId)
    i18nSupplementedEvent NewEncryptedAmount{..} = "New encrypted amount added to account " <> descrAccount neaAccount
    i18nSupplementedEvent EncryptedAmountsRemoved{..} = "Consumed encrypted amounts on account " <> descrAccount earAccount
    i18nSupplementedEvent EncryptedSelfAmountAdded{..} = "Updated shielded balance of account " <> descrAccount eaaAccount
    i18nSupplementedEvent AmountAddedByDecryption{..} = "Unshielded " <> descrAmount aabdAmount <> " on account " <> descrAccount aabdAccount
    i18nSupplementedEvent UpdateEnqueued{} = "Chain update event enqueued."
    i18nSupplementedEvent TransferredWithSchedule{..} = "Transferred with schedule from " <> descrAccount etwsFrom <> " to " <> descrAccount etwsTo
    i18nSupplementedEvent CredentialsUpdated{..} = "Credentials on account " <> descrAccount cuAccount <> " updated."
    i18nSupplementedEvent DataRegistered{} = "Data registered on the chain."
    i18nSupplementedEvent TransferMemo{..} = "Memo '" <> Text.pack (show tmMemo) <> "' included in a transfer." -- TODO: This would ideally try to render the Memo in a readable way, if it is a valid string or integer, say.
    i18nSupplementedEvent Interrupted{..} = "Execution of " <> descrInstance iAddress <> " triggered an operation."
    i18nSupplementedEvent Resumed{..} | rSuccess = "Operation succeeded and execution of " <> descrInstance rAddress <> " resumed."
    i18nSupplementedEvent Resumed{..} | otherwise = "Operation failed and execution of " <> descrInstance rAddress <> " resumed."
    i18nSupplementedEvent Upgraded{..} = "Smart contract instance at " <> descrInstance euAddress <> " was upgraded from " <> descrModule euFrom <> " to " <> descrModule euTo <> "."
    i18nSupplementedEvent BakerSetOpenStatus{..} = "Open status of validator " <> descrBaker ebsosBakerId ebsosAccount <> " set to " <> descrOpenStatus ebsosOpenStatus
    i18nSupplementedEvent BakerSetMetadataURL{..} = "Metadata URL of validator " <> descrBaker ebsmuBakerId ebsmuAccount <> " set to " <> descrMetadataURL ebsmuMetadataURL
    i18nSupplementedEvent BakerSetTransactionFeeCommission{..} = "Transaction fee commission of validator " <> descrBaker ebstfcBakerId ebstfcAccount <> " set to " <> descrAmountFraction ebstfcTransactionFeeCommission
    i18nSupplementedEvent BakerSetBakingRewardCommission{..} = "Block reward commission of validator " <> descrBaker ebsbrcBakerId ebsbrcAccount <> " set to " <> descrAmountFraction ebsbrcBakingRewardCommission
    i18nSupplementedEvent BakerSetFinalizationRewardCommission{..} = "Finalization reward commission of validator " <> descrBaker ebsfrcBakerId ebsfrcAccount <> " set to " <> descrAmountFraction ebsfrcFinalizationRewardCommission
    i18nSupplementedEvent DelegationStakeIncreased{..} = "Stake of delegator " <> descrDelegator edsiDelegatorId edsiAccount <> " increased to " <> descrAmount edsiNewStake
    i18nSupplementedEvent DelegationStakeDecreased{..} = "Stake of delegator " <> descrDelegator edsdDelegatorId edsdAccount <> " decreased to " <> descrAmount edsdNewStake
    i18nSupplementedEvent DelegationSetRestakeEarnings{..} = "Delegator " <> descrDelegator edsreDelegatorId edsreAccount <> if edsreRestakeEarnings then " set to restake earnings." else " unset restaking of earnings."
    i18nSupplementedEvent DelegationSetDelegationTarget{..} =
        "Delegator "
            <> descrDelegator edsdtDelegatorId edsdtAccount
            <> " set delegation target to "
            <> case edsdtDelegationTarget of
                DelegatePassive -> "passive delegation"
                DelegateToBaker bid -> "staking pool " <> Text.pack (show bid)
    i18nSupplementedEvent DelegationAdded{..} = "Added delegator " <> descrDelegator edaDelegatorId edaAccount
    i18nSupplementedEvent DelegationRemoved{..} = "Removed delegator " <> descrDelegator edrDelegatorId edrAccount
    i18nSupplementedEvent BakerSuspended{..} = "Validator " <> descrBaker ebsBakerId ebsAccount <> " was suspended."
    i18nSupplementedEvent BakerResumed{..} = "Validator " <> descrBaker ebrBakerId ebrAccount <> " was resumed."
    i18nSupplementedEvent (TokenModuleEvent tokenId type' _) = Text.pack (show tokenId) <> " token module event occurred of type " <> Text.pack (show type') <> "." -- TODO: It would be ideally to render the `details` in a readable way in the wallets.
    i18nSupplementedEvent (TokenTransfer tokenId from to amount memo) = Text.pack (tokenAmountToString amount) <> " " <> Text.pack (show tokenId) <> " transferred from " <> Text.pack (show from) <> " to " <> Text.pack (show to) <> maybe "" (\m -> " with memo " <> Text.pack (show m)) memo <> "." -- TODO: It would be ideally to render the `memo` in a readable way in the wallets.
    i18nSupplementedEvent TokenMint{..} = Text.pack (tokenAmountToString etmAmount) <> " " <> Text.pack (show etmTokenId) <> " minted to " <> Text.pack (show etmTarget) <> "."
    i18nSupplementedEvent TokenBurn{..} = Text.pack (tokenAmountToString etbAmount) <> " " <> Text.pack (show etbTokenId) <> " burned from " <> Text.pack (show etbTarget) <> "."
    i18nSupplementedEvent TokenCreated{..} = "Token created: " <> Text.pack (showPrettyJSON etcPayload)
    i18nSpecialEvent BakingRewards{..} =
        "Block rewards\n"
            <> Text.unlines (map (\(addr, amnt) -> "  - account " <> descrAccount addr <> " awarded " <> descrAmount amnt) . Map.toAscList . accountAmounts $ stoBakerRewards)
    i18nSpecialEvent Mint{..} =
        "New CCD minted\n "
            <> Text.unlines
                [ "  - " <> descrAmount stoMintBakingReward <> " to the validator reward account",
                  "  - " <> descrAmount stoMintFinalizationReward <> " to the finalization reward account",
                  "  - " <> descrAmount stoMintPlatformDevelopmentCharge <> " as the platform development charge to account " <> descrAccount stoFoundationAccount
                ]
    i18nSpecialEvent FinalizationRewards{..} =
        "Finalization rewards\n "
            <> Text.unlines (map (\(addr, amnt) -> "  - account " <> descrAccount addr <> " awarded " <> descrAmount amnt) . Map.toAscList . accountAmounts $ stoFinalizationRewards)
    i18nSpecialEvent BlockReward{..} =
        "Block rewards\n"
            <> Text.unlines
                [ "  - " <> descrAmount stoTransactionFees <> " transaction fees in the block",
                  "  - " <> descrAmount stoOldGASAccount <> " was the old balance of the GAS account",
                  "  - " <> descrAmount stoNewGASAccount <> " is the new balance of the GAS account",
                  "  - " <> descrAmount stoBakerReward <> " awarded to the block validator to address " <> descrAccount stoBaker,
                  "  - " <> descrAmount stoFoundationCharge <> " awarded to the foundation account " <> descrAccount stoFoundationAccount
                ]
    i18nSpecialEvent PaydayFoundationReward{..} =
        "Foundation development charge payout: "
            <> descrAmount stoDevelopmentCharge
            <> " to "
            <> descrAccount stoFoundationAccount
    i18nSpecialEvent PaydayAccountReward{..} =
        "Reward payout of "
            <> descrAmount total
            <> " to "
            <> descrAccount stoAccount
            <> ": \n"
            <> Text.unlines
                [ "  - " <> descrAmount stoTransactionFees <> " from transaction fees",
                  "  - " <> descrAmount stoBakerReward <> " from block rewards",
                  "  - " <> descrAmount stoFinalizationReward <> " from finalization rewards"
                ]
      where
        total = stoTransactionFees + stoBakerReward + stoFinalizationReward
    i18nSpecialEvent BlockAccrueReward{..} =
        "Block rewards\n"
            <> Text.unlines
                [ "  - " <> descrAmount stoTransactionFees <> " transaction fees in the block",
                  "  - " <> descrAmount stoOldGASAccount <> " was the old balance of the GAS account",
                  "  - " <> descrAmount stoNewGASAccount <> " is the new balance of the GAS account",
                  "  - " <> descrAmount stoBakerReward <> " accrued to pool with ID " <> descrBakerId stoBakerId,
                  "  - " <> descrAmount stoPassiveReward <> " accrued by passive delegation",
                  "  - " <> descrAmount stoFoundationCharge <> " accrued to the foundation"
                ]
    i18nSpecialEvent PaydayPoolReward{..} =
        "Payday rewards to "
            <> poolName
            <> ":\n"
            <> Text.unlines
                [ "  - " <> descrAmount stoTransactionFees <> " from transaction fees",
                  "  - " <> descrAmount stoBakerReward <> " from block rewards",
                  "  - " <> descrAmount stoFinalizationReward <> " from finalization rewards"
                ]
      where
        poolName = maybe "passive delegators" (\bid -> "pool with ID " <> descrBakerId bid) stoPoolOwner
    i18nSpecialEvent ValidatorPrimedForSuspension{..} =
        "Validator "
            <> descrBaker vpfsBakerId vpfsAccount
            <> " was primed for suspension at the next pay day."
    i18nSpecialEvent ValidatorSuspended{..} =
        "Validator "
            <> descrBaker vsBakerId vsAccount
            <> " was suspended due to inactivity."

    i18nSpecialOutcomeShort BakingRewards{} = "Block rewards"
    i18nSpecialOutcomeShort Mint{} = "New CCD minted"
    i18nSpecialOutcomeShort FinalizationRewards{} = "Finalization rewards"
    i18nSpecialOutcomeShort BlockReward{} = "Block rewards"
    i18nSpecialOutcomeShort PaydayFoundationReward{} = "Foundation development charge payout"
    i18nSpecialOutcomeShort PaydayAccountReward{} = "Reward payout"
    i18nSpecialOutcomeShort BlockAccrueReward{} = "Accrual of block rewards"
    i18nSpecialOutcomeShort PaydayPoolReward{} = "Pool rewards"
    i18nSpecialOutcomeShort ValidatorPrimedForSuspension{} = "Validator primed for suspension"
    i18nSpecialOutcomeShort ValidatorSuspended{} = "Validator suspended"

    i18nErrorMessage (EMErrorResponse NotFound) = "Not found"
    i18nErrorMessage (EMErrorResponse InternalError{}) = "Internal server error"
    i18nErrorMessage (EMErrorResponse InvalidArgs{}) = "Invalid arguments"
    i18nErrorMessage (EMErrorResponse NotAuthenticated) = "Not logged in"
    i18nErrorMessage (EMErrorResponse (PermissionDenied reason)) = "Permission denied: " <> reason
    i18nErrorMessage (EMErrorResponse (BadMethod _)) = "Bad method"
    i18nErrorMessage EMGRPCError = "Cannot communicate with node via GRPC endpoint."
    i18nErrorMessage (EMGRPCErrorResponse s) = "GRPC call failed: " <> Text.pack s
    i18nErrorMessage (EMParseError s) = "Parsing input failed: " <> Text.pack s
    i18nErrorMessage EMCredentialRejected = "Credential rejected by the node"
    i18nErrorMessage EMTransactionRejected = "Transaction rejected by the node"
    i18nErrorMessage EMMalformedTransaction = "Malformed transaction hash"
    i18nErrorMessage EMMalformedAddress = "Malformed account address"
    i18nErrorMessage EMDatabaseError = "Database error"
    i18nErrorMessage EMAccountNotFinal = "Account creation is not finalized"
    i18nErrorMessage EMConfigurationError = "Server configuration error"
    i18nErrorMessage EMAccountDoesNotExist = "Account does not exist"
    i18nErrorMessage EMMissingParameter = "Missing parameter"
    i18nErrorMessage EMActionNotCurrentlySupported = "The required action is not supported. The node's protocol version is incompatible with it."
    i18nErrorMessage EMInvokeFailed = "Invoking a contract failed."
    i18nErrorMessage EMV0Contract = "Invoking a V0 contract is not supported."
    i18nErrorMessage EMBadGateway = "Invalid response from upstream server."
    i18nErrorMessage EMGatewayTimeout = "Timeout occurred waiting for response from upstream server."
