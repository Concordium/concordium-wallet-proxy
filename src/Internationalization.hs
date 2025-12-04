{-# LANGUAGE OverloadedStrings #-}

module Internationalization (I18n, ErrorMessage (..), module Internationalization) where

import Control.Arrow
import Data.Text (Text)
import Yesod

import Concordium.Types.Execution
import Concordium.Types.Transactions

import Internationalization.Base
import qualified Internationalization.En as En

newtype ShortDescription a = ShortDescription a

class Internationalizable a where
    i18n :: I18n -> a -> Text

instance Internationalizable RejectReason where
    i18n = i18nRejectReason

instance Internationalizable TransactionSummaryType where
    i18n i (TSTAccountTransaction (Just t)) = i18nTransactionType i t
    i18n i (TSTAccountTransaction Nothing) = i18nMalformedTransaction i
    i18n i (TSTCredentialDeploymentTransaction t) = i18nDeployCredential i t
    i18n i (TSTUpdateTransaction t) = i18nUpdateTransaction i t

instance Internationalizable Event where
    i18n = i18nEvent

instance Internationalizable SupplementedEvent where
    i18n = i18nSupplementedEvent

instance Internationalizable SpecialTransactionOutcome where
    i18n = i18nSpecialEvent

instance Internationalizable (ShortDescription SpecialTransactionOutcome) where
    i18n i (ShortDescription a) = i18nSpecialOutcomeShort i a

instance Internationalizable ErrorMessage where
    i18n = i18nErrorMessage

defaultTranslation :: (Text, I18n)
defaultTranslation = ("en", En.translation)

getTranslation :: Text -> Maybe I18n
getTranslation "en" = Just En.translation
getTranslation _ = Nothing

-- | Get a function for rendering internationalizable values according to the
--  client's requested language, setting the content language to indicate the
--  response language.
internationalize :: (MonadHandler m) => m I18n
internationalize = do
    langs <- languages
    let (langCode, tr) = case [(c, t) | (c, Just t) <- (id &&& getTranslation) <$> langs] of
            [] -> defaultTranslation
            (h : _) -> h
    replaceOrAddHeader "Content-Language" langCode
    return tr
