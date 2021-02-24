{-# LANGUAGE OverloadedStrings #-}
module Internationalization.En where

import qualified Data.Text as Text
import Yesod

import qualified Data.Map.Strict as Map
import Concordium.Types.Transactions
import Concordium.Types.Execution
import Concordium.Types.Updates

import Internationalization.Base
import Concordium.ID.Types
import Concordium.Wasm (contractAndFunctionName, initContractName, parameter)

translation :: I18n
translation = I18n {..}
    where
        i18nMalformedTransaction = "Malformed transaction body"
        i18nUpdateTransaction UpdateAuthorization = "Update the authorizations access structure"
        i18nUpdateTransaction UpdateProtocol = "Protocol update"
        i18nUpdateTransaction UpdateElectionDifficulty = "Update election difficulty"
        i18nUpdateTransaction UpdateEuroPerEnergy = "Update Euro per Energy exchange rate"
        i18nUpdateTransaction UpdateMicroGTUPerEuro = "Update micro GTU per Euro exchange rate"
        i18nUpdateTransaction UpdateFoundationAccount = "Update the foundation account address"
        i18nUpdateTransaction UpdateMintDistribution = "Update parameters of the mint distribution"
        i18nUpdateTransaction UpdateTransactionFeeDistribution = "Update transaction fee distribution"
        i18nUpdateTransaction UpdateGASRewards = "Update parameters for GAS rewards distribution"

        i18nRejectReason ModuleNotWF = "Typechecking of module failed"
        i18nRejectReason (ModuleHashAlreadyExists mref) = "A module with the hash " <> descrModule mref <> " already exists"
        i18nRejectReason (InvalidAccountReference addr) = "The account " <> descrAccount addr <> " does not exist"
        i18nRejectReason (InvalidModuleReference mref) = "Module does not exist: " <> descrModule mref
        i18nRejectReason (InvalidContractAddress caddr) = "No smart contract instance exists with address " <> descrInstance caddr
        i18nRejectReason (ReceiverAccountNoCredential addr) = "The receiving account (" <> descrAccount addr <> ") has has no valid credential"
        i18nRejectReason (ReceiverContractNoCredential caddr) = "The receiving smart contract instance (" <> descrInstance caddr <> "') has no valid credential"
        i18nRejectReason (AmountTooLarge addr _) = "The sending account (" <> descrAddress addr <> ") has insufficient funds"
        i18nRejectReason SerializationFailure = "Malformed transaction body"
        i18nRejectReason OutOfEnergy = "Insufficient energy"
        i18nRejectReason Rejected = "Rejected by contract logic"
        i18nRejectReason (NonExistentRewardAccount addr) = "The designated reward account (" <> descrAccount addr <> ") does not exist"
        i18nRejectReason InvalidProof = "Invalid proof"
        i18nRejectReason (InvalidInitMethod mref initName) = "Init method " <> descrInitName initName <> " does not exist in module " <> descrModule mref <> "."
        i18nRejectReason (InvalidReceiveMethod mref receiveName) = "Receive method " <> descrReceiveName receiveName <> " does not exist for module " <> descrModule mref <> "."
        i18nRejectReason RuntimeFailure = "Runtime failure when executing smart contract."
        i18nRejectReason (DuplicateAggregationKey _) = "Duplicate aggregation key."
        i18nRejectReason NonExistentAccountKey = "The account key with the specified index does not exist."
        i18nRejectReason KeyIndexAlreadyInUse = "The requested key index is already in use."
        i18nRejectReason InvalidAccountKeySignThreshold = "The requested sign threshold would exceed the number of keys on the account."
        i18nRejectReason InvalidEncryptedAmountTransferProof = "The encrypted amount transfer has an invalid proof."
        i18nRejectReason (EncryptedAmountSelfTransfer _) = "An encrypted amount transfer from the account to itself is not allowed."
        i18nRejectReason InvalidTransferToPublicProof  = "The secret to public transfer has an invalid proof."
        i18nRejectReason InvalidIndexOnEncryptedTransfer = "The provided encryped amount index is out of bounds."
        i18nRejectReason ZeroScheduledAmount = "Attempt to transfer 0 GTU with schedule."
        i18nRejectReason NonIncreasingSchedule = "Attempt to transfer amount with non-increasing schedule."
        i18nRejectReason FirstScheduledReleaseExpired = "The first scheduled release is in the past."
        i18nRejectReason (ScheduledSelfTransfer _) = "Attempt to transfer from account A to A with schedule."
        i18nRejectReason (AlreadyABaker bid) = "Baker with ID " <> Text.pack (show bid) <> " already exists."
        i18nRejectReason (NotABaker addr) = "Account " <> descrAccount addr <> " is not a baker."
        i18nRejectReason InsufficientBalanceForBakerStake = "Sender account has insufficient balance to cover the requested stake."
        i18nRejectReason BakerInCooldown = "Request to make change to the baker while the baker is in the cooldown period."

        i18nTransactionType TTDeployModule = "Deploy module"
        i18nTransactionType TTInitContract = "Initialize smart contract"
        i18nTransactionType TTUpdate = "Invoke smart contract"
        i18nTransactionType TTTransfer = "Transfer"
        i18nTransactionType TTUpdateBakerStake = "Update baker stake"
        i18nTransactionType TTUpdateBakerKeys = "Update baker keys"
        i18nTransactionType TTUpdateBakerRestakeEarnings = "Update whether to restake baker earnings"
        i18nTransactionType TTAddBaker = "Add baker"
        i18nTransactionType TTRemoveBaker = "Remove baker"
        i18nTransactionType TTUpdateAccountKeys = "Update account keys"
        i18nTransactionType TTAddAccountKeys = "Add account keys"
        i18nTransactionType TTRemoveAccountKeys = "Remove account keys"
        i18nTransactionType TTEncryptedAmountTransfer = "Shielded transfer"
        i18nTransactionType TTTransferToEncrypted = "Shielded amount"
        i18nTransactionType TTTransferToPublic = "Unshielded amount"
        i18nTransactionType TTTransferWithSchedule = "Transfer with schedule"

        i18nDeployCredential Initial = "Deploy initial account credential"
        i18nDeployCredential Normal = "Deploy account credential"

        i18nEvent (ModuleDeployed mref) = "Deployed module " <> descrModule mref
        i18nEvent ContractInitialized{..} =
          "Initialized smart contract " <>
          initContractName ecInitName <>
          " at address " <> descrInstance ecAddress <>
          " with balance " <>
          descrAmount ecAmount <>
          ". " <>
          Text.pack (show (length ecEvents)) <> " events were logged"
        i18nEvent Updated{..} =
          let (cname, fname) = contractAndFunctionName euReceiveName in
          "Invoked smart contract: source=" <> descrAddress euInstigator <>
          ", target=" <> descrInstance euAddress <>
          ", contract= " <> cname <>
          ", function= " <> fname <>
          ", amount=" <> descrAmount euAmount <>
          ", message=" <> Text.pack (show (parameter euMessage))
        i18nEvent (Transferred sender amt recv) = "Transferred " <> descrAmount amt <> " from " <> descrAddress sender <> " to " <> descrAddress recv
        i18nEvent (AccountCreated addr) = "Created account with address " <> descrAccount addr
        i18nEvent (CredentialDeployed _ addr) = "Deployed a credential to account " <> descrAccount addr
        i18nEvent BakerAdded{..} = "Added baker " <> descrBaker ebaBakerId ebaAccount <> " and initial stake " <> descrAmount ebaStake
        i18nEvent BakerRemoved{..} = "Removed baker " <> descrBaker ebrBakerId ebrAccount
        i18nEvent BakerStakeIncreased{..} = "Stake of baker " <> descrBaker ebsiBakerId ebsiAccount <> " increased to " <> descrAmount ebsiNewStake
        i18nEvent BakerStakeDecreased{..} = "Stake of baker " <> descrBaker ebsiBakerId ebsiAccount <> " decreased to " <> descrAmount ebsiNewStake
        i18nEvent BakerSetRestakeEarnings{..} = "Baker " <> descrBaker ebsreBakerId ebsreAccount <> if ebsreRestakeEarnings then " set to restake earnings." else " unset restaking of earnings."
        i18nEvent BakerKeysUpdated{..} = "Baker " <> descrBaker ebkuBakerId ebkuAccount <> " keys updated."
        i18nEvent AccountKeysUpdated = "Updated account keys"
        i18nEvent AccountKeysAdded = "Added account keys"
        i18nEvent AccountKeysRemoved = "Removed account keys"
        i18nEvent AccountKeysSignThresholdUpdated = "Signature threshold updated"
        i18nEvent NewEncryptedAmount{..} = "New encrypted amount added to account " <> descrAccount neaAccount
        i18nEvent EncryptedAmountsRemoved{..} = "Consumed encrypted amounts on account " <> descrAccount earAccount
        i18nEvent EncryptedSelfAmountAdded{..} = "Updated shielded balance of account " <> descrAccount eaaAccount
        i18nEvent AmountAddedByDecryption{..} = "Unshielded " <> descrAmount aabdAmount <> " on account " <> descrAccount aabdAccount
        i18nEvent UpdateEnqueued{..} = "Chain update event enqueued."
        i18nEvent TransferredWithSchedule{..} = "Transferred with schedule from " <> descrAccount etwsFrom <> " to " <> descrAccount etwsTo

        i18nSpecialEvent BakingRewards{..} = "Baking rewards\n" <>
            Text.unlines (map (\(addr, amnt) -> "  - account " <> descrAccount addr <> " awarded " <> descrAmount amnt) . Map.toAscList . accountAmounts $ stoBakerRewards)
        i18nSpecialEvent Mint{..} = "New GTU minted\n " <>
            Text.unlines [
              "  - " <> descrAmount stoMintBakingReward <> " to the baking reward account",
              "  - " <> descrAmount stoMintFinalizationReward <> " to the finalization reward account",
                "  - " <> descrAmount stoMintPlatformDevelopmentCharge <> " as the platform development charge to account " <> descrAccount stoFoundationAccount]
        i18nSpecialEvent FinalizationRewards{..} = "Finalization rewards\n " <>
            Text.unlines (map (\(addr, amnt) -> "  - account " <> descrAccount addr <> " awarded " <> descrAmount amnt) . Map.toAscList . accountAmounts $ stoFinalizationRewards)
        i18nSpecialEvent BlockReward{..} = "Block rewards\n" <>
            Text.unlines [
              "  - " <> descrAmount stoTransactionFees <> " transaction fees in the block",
              "  - " <> descrAmount stoOldGASAccount <> " was the old balance of the GAS account",
              "  - " <> descrAmount stoNewGASAccount <> " is the new balance of the GAS account",
              "  - " <> descrAmount stoBakerReward <> " awarded to the block baker to address " <> descrAccount stoBaker,
              "  - " <> descrAmount stoFoundationCharge <> " awarded to the foundation account " <> descrAccount stoFoundationAccount
              ]

        i18nSpecialOutcomeShort BakingRewards{..} = "Baking rewards"
        i18nSpecialOutcomeShort Mint{..} = "New GTU minted"
        i18nSpecialOutcomeShort FinalizationRewards{..} = "Finalization rewards"
        i18nSpecialOutcomeShort BlockReward{..} = "Block rewards"

        i18nErrorMessage (EMErrorResponse NotFound) = "Not found"
        i18nErrorMessage (EMErrorResponse InternalError{}) = "Internal server error"
        i18nErrorMessage (EMErrorResponse InvalidArgs{}) = "Invalid arguments"
        i18nErrorMessage (EMErrorResponse NotAuthenticated) = "Not logged in"
        i18nErrorMessage (EMErrorResponse (PermissionDenied reason)) = "Permission denied: " <> reason
        i18nErrorMessage (EMErrorResponse (BadMethod _)) = "Bad method"
        i18nErrorMessage EMGRPCError = "Cannot communicate with node via GRPC endpoint"
        i18nErrorMessage (EMParseError s) = "Parsing input failed: " <> Text.pack (show s)
        i18nErrorMessage EMCredentialRejected = "Credential rejected by the node"
        i18nErrorMessage EMTransactionRejected = "Transaction rejected by the node"
        i18nErrorMessage EMMalformedTransaction = "Malformed transaction hash"
        i18nErrorMessage EMMalformedAddress = "Malformed account address"
        i18nErrorMessage EMDatabaseError = "Database error"
        i18nErrorMessage EMAccountNotFinal = "Account creation is not finalized"
        i18nErrorMessage EMConfigurationError = "Server configuration error"
        i18nErrorMessage EMAccountDoesNotExist = "Account does not exist"
        i18nErrorMessage EMMissingParameter = "Missing parameter"
