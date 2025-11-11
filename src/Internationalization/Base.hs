{-# LANGUAGE OverloadedStrings #-}

module Internationalization.Base where

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Yesod

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import qualified Concordium.Wasm as Wasm

import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types (CredentialType)
import Concordium.Types.Updates (UpdateType)

data ErrorMessage
    = EMErrorResponse ErrorResponse
    | -- | The wallet proxy could not communicate with the node.
      EMGRPCError
    | -- | The node rejected the call. We report the reason back to the caller.
      EMGRPCErrorResponse String
    | EMParseError String
    | EMCredentialRejected
    | EMTransactionRejected
    | EMMalformedTransaction
    | EMMalformedAddress
    | EMDatabaseError
    | EMAccountNotFinal
    | EMConfigurationError
    | EMAccountDoesNotExist
    | EMMissingParameter
    | -- | Action not supported due to the node protocol version not allowing it.
      EMActionNotCurrentlySupported
    | -- | Invoke of a smart contract failed with the given reason.
      EMInvokeFailed
    | -- | Expected a V1 contract, but a V0 contract was given.
      EMV0Contract
    | -- | Bad gateway.
      EMBadGateway
    | -- | Gateway time-out.
      EMGatewayTimeout

data I18n = I18n
    { i18nRejectReason :: RejectReason -> Text,
      i18nTransactionType :: TransactionType -> Text,
      i18nMalformedTransaction :: Text,
      i18nDeployCredential :: CredentialType -> Text,
      i18nEvent :: Event -> Text,
      i18nUpdateTransaction :: UpdateType -> Text,
      i18nSpecialEvent :: SpecialTransactionOutcome -> Text,
      i18nSpecialOutcomeShort :: SpecialTransactionOutcome -> Text,
      i18nErrorMessage :: ErrorMessage -> Text
    }

descrModule :: ModuleRef -> Text
descrModule = Text.pack . show

descrInitName :: Wasm.InitName -> Text
descrInitName = Text.pack . show

descrReceiveName :: Wasm.ReceiveName -> Text
descrReceiveName = Text.pack . show

descrAccount :: AccountAddress -> Text
descrAccount = Text.pack . show

descrInstance :: ContractAddress -> Text
descrInstance = Text.pack . show

descrOpenStatus :: OpenStatus -> Text
descrOpenStatus = Text.pack . show

descrMetadataURL :: UrlText -> Text
descrMetadataURL (UrlText t) = t

descrAmountFraction :: AmountFraction -> Text
descrAmountFraction = Text.pack . show

descrAmount :: Amount -> Text
descrAmount amount = pack (amountToString amount) <> " CCD"

descrBakerId :: BakerId -> Text
descrBakerId = Text.pack . show

descrBaker :: BakerId -> AccountAddress -> Text
descrBaker bid addr = Text.pack $ show addr ++ " (ID " ++ show bid ++ ")"

descrDelegator :: DelegatorId -> AccountAddress -> Text
descrDelegator did addr = Text.pack $ show addr ++ " (ID " ++ show did ++ ")"

descrAddress :: Address -> Text
descrAddress (AddressAccount addr) = descrAccount addr
descrAddress (AddressContract caddr) = descrInstance caddr

descrEncryptedAmountIndex :: EncryptedAmountIndex -> Text
descrEncryptedAmountIndex = Text.pack . show

descrEncryptedAmountAggIndex :: EncryptedAmountAggIndex -> Text
descrEncryptedAmountAggIndex = Text.pack . show
