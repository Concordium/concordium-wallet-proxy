{-# LANGUAGE OverloadedStrings #-}
module Internationalization.Base where

import Data.Text(Text,pack)
import qualified Data.Text as Text
import Yesod

import qualified Concordium.Wasm as Wasm
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.Execution

import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types (CredentialType)
import Concordium.Types.Updates (UpdateType)

data ErrorMessage
    = EMErrorResponse ErrorResponse
    | EMGRPCError
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
    -- |Action not supported due to the node protocol version not allowing it.
    | EMActionNotCurrentlySupported
    | EMDuplicate

data I18n = I18n {
    i18nRejectReason :: RejectReason -> Text,
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

descrAmount :: Amount -> Text
descrAmount amount = pack (amountToString amount) <> " GTU"

descrBaker :: BakerId -> AccountAddress -> Text
descrBaker bid addr = Text.pack $ show addr ++ "(ID " ++ show bid ++ ")"

descrAddress :: Address -> Text
descrAddress (AddressAccount addr) = descrAccount addr
descrAddress (AddressContract caddr) = descrInstance caddr

descrEncryptedAmountIndex :: EncryptedAmountIndex -> Text
descrEncryptedAmountIndex = Text.pack . show

descrEncryptedAmountAggIndex :: EncryptedAmountAggIndex -> Text
descrEncryptedAmountAggIndex = Text.pack . show
