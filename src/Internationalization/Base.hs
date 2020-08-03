{-# LANGUAGE OverloadedStrings #-}
module Internationalization.Base where

import Data.Text(Text,pack)
import qualified Data.Text as Text
import Yesod

import Concordium.Types
import Concordium.Types.Transactions
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Execution

data ErrorMessage
    = EMErrorResponse ErrorResponse
    | EMGRPCError
    | EMParseError String
    | EMCredentialRejected
    | EMMalformedTransaction
    | EMMalformedAddress
    | EMDatabaseError
    | EMAccountNotFinal
    | EMConfigurationError

data I18n = I18n {
    i18nRejectReason :: RejectReason -> Text,
    i18nTransactionType :: TransactionType -> Text,
    i18nDeployCredential :: Text,
    i18nEvent :: Event -> Text,
    i18nSpecialEvent :: SpecialTransactionOutcome -> Text,
    i18nSpecialOutcomeShort :: SpecialTransactionOutcome -> Text,
    i18nErrorMessage :: ErrorMessage -> Text
}

descrModule :: Core.ModuleRef -> Text
descrModule = Text.pack . show

descrContractRef :: Core.ModuleRef -> Core.TyName -> Text
descrContractRef mref (Core.TyName tyname) = descrModule mref <> ":" <> Text.pack (show tyname)

descrAccount :: AccountAddress -> Text
descrAccount = Text.pack . show

descrInstance :: ContractAddress -> Text
descrInstance = Text.pack . show

descrAmount :: Amount -> Text
descrAmount amount = pack (amountToString amount)

descrBaker :: BakerId -> Text
descrBaker = Text.pack . show

descrAddress :: Address -> Text
descrAddress (AddressAccount addr) = descrAccount addr
descrAddress (AddressContract caddr) = descrInstance caddr
