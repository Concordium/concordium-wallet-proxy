{-# LANGUAGE OverloadedStrings #-}
module Internationalization.En where

import qualified Data.Text as Text
import Yesod

import Concordium.Types.Transactions
import Concordium.Types.Execution

import Internationalization.Base


translation :: I18n
translation = I18n {..}
    where
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
        i18nRejectReason (RemovingNonExistentBaker bid) = "Baker does not exist: " <> descrBaker bid
        i18nRejectReason (InvalidBakerRemoveSource _) = "Sender is not authorized to remove baker"
        i18nRejectReason (UpdatingNonExistentBaker bid) = "Baker does not exist: " <> descrBaker bid
        i18nRejectReason (InvalidStakeDelegationTarget bid) = "Baker does not exist: " <> descrBaker bid
        i18nRejectReason (DuplicateSignKey _) = "Duplicate baker signature key"
        i18nRejectReason (NotFromBakerAccount _ _) = "Sender is not the baker's designated account"
        i18nRejectReason NotFromSpecialAccount = "Sender is not authorized to perform chain control actions"
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

        i18nTransactionType TTDeployModule = "Deploy module"
        i18nTransactionType TTInitContract = "Initialize smart contract"
        i18nTransactionType TTUpdate = "Invoke smart contract"
        i18nTransactionType TTTransfer = "Transfer"
        i18nTransactionType TTAddBaker = "Add baker"
        i18nTransactionType TTRemoveBaker = "Remove baker"
        i18nTransactionType TTUpdateBakerAccount = "Update baker account"
        i18nTransactionType TTUpdateBakerSignKey = "Update baker key"
        i18nTransactionType TTDelegateStake = "Delegate stake"
        i18nTransactionType TTUndelegateStake = "Undelegate stake"
        i18nTransactionType TTUpdateBakerAggregationVerifyKey = "Update baker aggregation key"
        i18nTransactionType TTUpdateBakerElectionKey = "Update baker election key"
        i18nTransactionType TTUpdateAccountKeys = "Update account keys"
        i18nTransactionType TTAddAccountKeys = "Add account keys"
        i18nTransactionType TTRemoveAccountKeys = "Remove account keys"
        i18nTransactionType TTEncryptedAmountTransfer = "Shielded transfer"
        i18nTransactionType TTTransferToEncrypted = "Shielded amount"
        i18nTransactionType TTTransferToPublic = "Unshielded amount"

        i18nDeployCredential = "Deploy account credential"

        i18nEvent (ModuleDeployed mref) = "Deployed module " <> descrModule mref
        i18nEvent ContractInitialized{..} =
          "Initialized smart contract " <>
          descrModule ecRef <>
          " at address " <> descrInstance ecAddress <>
          " with balance " <>
          descrAmount ecAmount <>
          ". " <>
          Text.pack (show (length ecEvents)) <> " events were logged"
        i18nEvent Updated{..} =
          "Invoked smart contract: source=" <> descrAddress euInstigator <>
          ", target=" <> descrInstance euAddress <>
          ", amount=" <> descrAmount euAmount <>
          ", message=" <> Text.pack (show euMessage)
        i18nEvent (Transferred sender amt recv) = "Transferred " <> descrAmount amt <> " from " <> descrAddress sender <> " to " <> descrAddress recv
        i18nEvent (AccountCreated addr) = "Created account with address " <> descrAccount addr
        i18nEvent (CredentialDeployed _ addr) = "Deployed a credential to account " <> descrAccount addr
        i18nEvent (BakerAdded bid) = "Added baker " <> descrBaker bid
        i18nEvent (BakerRemoved bid) = "Removed baker " <> descrBaker bid
        i18nEvent (BakerAccountUpdated bid addr) = "Updated account for baker " <> descrBaker bid <> " to " <> descrAccount addr
        i18nEvent (BakerKeyUpdated bid _) = "Updated key for baker " <> descrBaker bid
        i18nEvent (BakerElectionKeyUpdated bid _) = "Updated election key for baker " <> descrBaker bid
        i18nEvent (StakeDelegated _ bid) = "Delegated stake to baker " <> descrBaker bid
        i18nEvent (StakeUndelegated _ _) = "Undelegated stake"
        i18nEvent (BakerAggregationKeyUpdated bid _) = "Updated aggregation key for baker " <> descrBaker bid
        i18nEvent AccountKeysUpdated = "Updated account keys"
        i18nEvent AccountKeysAdded = "Added account keys"
        i18nEvent AccountKeysRemoved = "Removed account keys"
        i18nEvent AccountKeysSignThresholdUpdated = "Signature threshold updated"
        i18nEvent NewEncryptedAmount{..} = "New encrypted amount added to account " <> descrAccount neaAccount
        i18nEvent EncryptedAmountsRemoved{..} = "Consumed encrypted amounts on account " <> descrAccount earAccount
        i18nEvent EncryptedSelfAmountAdded{..} = "Updated shielded balance of account " <> descrAccount eaaAccount
        i18nEvent AmountAddedByDecryption{..} = "Unshielded " <> descrAmount aabdAmount <> " on account " <> descrAccount aabdAccount
        i18nEvent UpdateEnqueued{..} = "Chain update event enqueued."

        i18nSpecialEvent BakingReward{..} = "Award " <> descrAmount stoRewardAmount <> " to baker " <> descrBaker stoBakerId <> " at " <> descrAccount stoBakerAccount

        i18nSpecialOutcomeShort BakingReward{..} = "Reward for baker " <> descrBaker stoBakerId

        i18nErrorMessage (EMErrorResponse NotFound) = "Not found"
        i18nErrorMessage (EMErrorResponse InternalError{}) = "Internal server error"
        i18nErrorMessage (EMErrorResponse InvalidArgs{}) = "Invalid arguments"
        i18nErrorMessage (EMErrorResponse NotAuthenticated) = "Not logged in"
        i18nErrorMessage (EMErrorResponse (PermissionDenied reason)) = "Permission denied: " <> reason
        i18nErrorMessage (EMErrorResponse (BadMethod _)) = "Bad method"
        i18nErrorMessage EMGRPCError = "Cannot communicate with node via GRPC endpoint"
        i18nErrorMessage (EMParseError s) = "Parsing input failed: " <> Text.pack (show s)
        i18nErrorMessage EMCredentialRejected = "Credential rejected by node"
        i18nErrorMessage EMMalformedTransaction = "Malformed transaction hash"
        i18nErrorMessage EMMalformedAddress = "Malformed account address"
        i18nErrorMessage EMDatabaseError = "Database error"
        i18nErrorMessage EMAccountNotFinal = "Account creation is not finalized"
        i18nErrorMessage EMConfigurationError = "Server configuration error"
        i18nErrorMessage EMAccountDoesNotExist = "Account does not exist"
        i18nErrorMessage EMMissingParameter = "Missing parameter"
