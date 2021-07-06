{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving, DerivingStrategies #-}
module Proxy where

import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString as BS

import qualified Data.Ratio as Rational
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Text.Read hiding (String)
import Control.Arrow (left)
import Control.Monad.Except
import Data.Functor
import Control.Exception (SomeException, catch)
import Data.Aeson(withObject, fromJSON, Result(..))
import Data.Aeson.Types(parse, parseMaybe, Pair, parseEither)
import Data.Aeson.Parser(json')
import qualified Data.Aeson as AE
import Data.Conduit(connect)
import qualified Data.Serialize as S
import Data.Conduit.Attoparsec  (sinkParserEither)
import Network.HTTP.Types(badRequest400, notFound404, badGateway502)
import Yesod hiding (InternalError)
import qualified Yesod
import Database.Persist.Sql
import Data.Maybe(catMaybes, fromMaybe, isJust)
import Data.Time.Clock.POSIX
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.PostgreSQL.JSON as EJ
import qualified Database.Esqueleto.Internal.Internal as EInternal
import System.Random
import Data.Foldable

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock as Clock

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Execution

import Concordium.Client.GRPC
import Concordium.Client.Types.Transaction(transferWithScheduleEnergyCost,
                                           transferWithSchedulePayloadSize,
                                           simpleTransferEnergyCost,
                                           simpleTransferPayloadSize,
                                           encryptedTransferEnergyCost,
                                           encryptedTransferPayloadSize,
                                           accountEncryptEnergyCost,
                                           accountEncryptPayloadSize,
                                           accountDecryptEnergyCost,
                                           accountDecryptPayloadSize,
                                           )
import Concordium.ID.Types (addressFromText, addressToBytes, KeyIndex, CredentialIndex)
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.Common.Version
import Concordium.SQL.AccountTransactionIndex
import Concordium.SQL.Helpers

import Internationalization

data ErrorCode = InternalError | RequestInvalid | DataNotFound
    deriving(Eq, Show, Enum)

data Proxy = Proxy {
  grpcEnvData :: !EnvData,
  dbConnectionPool :: ConnectionPool,
  gtuDropData :: Maybe GTUDropData,
  globalInfo :: Value,
  ipInfo :: Value
}

-- | Data needed for GTU drops.
data GTUDropData = GTUDropData {
  -- | Account to send GTU from.
  dropAccount :: AccountAddress,
  -- | Keys for the account.
  dropKeys :: [(CredentialIndex, [(KeyIndex, KeyPair)])]
  }

instance Yesod Proxy where
  -- Disable session handling entirely. We do not use sessions for anything at the moment.
  makeSessionBackend _ = return Nothing
  errorHandler e = do
    case e of
      Yesod.InternalError emsg -> $(logError) emsg
      _ -> return ()
    i <- internationalize
    return $ toTypedContent $ object [
        "errorMessage" .= i18n i (EMErrorResponse e),
        "error" .= fromEnum code
      ]
    where
      code = case e of
        NotFound -> RequestInvalid
        Yesod.InternalError{} -> InternalError
        InvalidArgs{} -> RequestInvalid
        NotAuthenticated -> RequestInvalid
        PermissionDenied{} -> RequestInvalid
        BadMethod{} -> RequestInvalid

-- Database table for GTU drop
share [mkPersist sqlSettings, mkMigrate "migrateGTURecipient"] [persistLowerCase|
    GTURecipient
        account (ByteStringSerialized AccountAddress)  maxlen=32
        transaction (ByteStringSerialized AccountTransaction)
        UniqueAccount account
|]

instance YesodPersist Proxy where
    type YesodPersistBackend Proxy = SqlBackend

    runDB action = do
        pool <- dbConnectionPool <$> getYesod
        runSqlPool action pool

defaultNetId :: Int
defaultNetId = 100

mkYesod "Proxy" [parseRoutes|
/v0/accBalance/#Text AccountBalanceR GET
/v0/accNonce/#Text AccountNonceR GET
/v0/accEncryptionKey/#Text AccountEncryptionKeyR GET
/v0/accTransactions/#Text AccountTransactionsR GET
/v0/transactionCost TransactionCostR GET
/v0/submissionStatus/#Text SubmissionStatusR GET
/v0/submitCredential/ CredentialR PUT
/v0/submitTransfer/ TransferR PUT
/v0/testnetGTUDrop/#Text GTUDropR PUT
/v0/global GlobalFileR GET
/v0/health HealthR GET
/v0/ip_info IpsR GET
|]

respond400Error :: ErrorMessage -> ErrorCode -> Handler TypedContent
respond400Error err code = do
  i <- internationalize
  sendResponseStatus badRequest400 $
    object ["errorMessage" .= i18n i err,
            "error" .= fromEnum code
           ]

respond404Error :: ErrorMessage -> Handler TypedContent
respond404Error err = do
  i <- internationalize
  sendResponseStatus notFound404 $
    object ["errorMessage" .= i18n i err,
            "error" .= fromEnum DataNotFound
           ]

runGRPC :: ClientMonad IO (Either String a) -> (a -> Handler TypedContent) -> Handler TypedContent
runGRPC c k = do
  cfg <- grpcEnvData <$> getYesod
  let
    exHandler :: SomeException -> IO (Either String a)
    exHandler = pure . Left . show
  liftIO ((left show <$> runClient cfg c) `catch` exHandler) >>= \case
    Left err -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack err
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMGRPCError,
        "error" .= fromEnum InternalError
        ]
    Right (Left err) -> do
      $(logError) $ "GRPC call failed: " <> Text.pack err
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMGRPCError,
        "error" .= fromEnum InternalError
        ]
    Right (Right a) -> k a


-- |Get the balance of an account.  If successful, the result is a JSON
-- object consisting of the following optional fields:
--   * "finalizedBalance": the balance of the account at the last finalized block
--   * "currentBalance": the balance of the account at the current best block
-- If neither field is present, the account does not currently exist.
-- The "finalizedBalance" field will be absent if the account has been created since
-- the last finalized block.
-- If the "finalizedBalance" field is present, then the "currentBalance" field will
-- also be present, since accounts cannot be deleted from the chain.
getAccountBalanceR :: Text -> Handler TypedContent
getAccountBalanceR addrText =
    runGRPC doGetBal $ \(lastFinInfo, bestInfo) -> do
      let
          getBal :: Value -> Maybe Value
          -- We're doing it in this low-level way to avoid parsing anything that
          -- is not needed, especially the encrypted amounts, since those are
          -- fairly expensive to parse.
          getBal (Object obj) = do
            publicAmount <- HM.lookup "accountAmount" obj
            encryptedAmount <- HM.lookup "accountEncryptedAmount" obj
            nnce <- HM.lookup "accountNonce" obj
            releases <- HM.lookup "accountReleaseSchedule" obj
            let staked = case HM.lookup "accountBaker" obj of
                  Nothing -> []
                  Just b -> ["accountBaker" .= b]
            return . object $ staked ++ ["accountAmount" .= publicAmount,
                                         "accountEncryptedAmount" .= encryptedAmount,
                                         "accountNonce" .= nnce,
                                         "accountReleaseSchedule" .= releases]
          getBal _ = Nothing

          lastFinBal = getBal lastFinInfo
          bestBal = getBal bestInfo
      $(logInfo) $ "Retrieved account balance for " <> addrText
                  <> ": finalizedBalance=" <> (Text.pack $ show lastFinBal)
                  <> ", currentBalance=" <> (Text.pack $ show bestBal)
      sendResponse $ object $ (maybe [] (\b -> ["finalizedBalance" .= b]) lastFinBal) <>
                        (maybe [] (\b -> ["currentBalance" .= b]) bestBal)
  where
    doGetBal = do
      status <- either fail return =<< getConsensusStatus
      lastFinBlock <- liftResult $ parse readLastFinalBlock status
      bestBlock <- liftResult $ parse readBestBlock status
      lastFinInfo <- either fail return =<< getAccountInfo addrText lastFinBlock
      bestInfo <- either fail return =<< getAccountInfo addrText bestBlock
      return $ Right (lastFinInfo, bestInfo)
    liftResult (Success s) = return s
    liftResult (Error err) = fail err

getAccountNonceR :: Text -> Handler TypedContent
getAccountNonceR addrText =
    runGRPC (getNextAccountNonce addrText) $ \v -> do
      $(logInfo) "Successfully got nonce."
      sendResponse v

-- |Get the account encryption key at the best block.
-- Return '404' status code if account does not exist in the best block at the moment.
getAccountEncryptionKeyR :: Text -> Handler TypedContent
getAccountEncryptionKeyR addrText = do
  runGRPC doGetEncryptionKey $ \accInfo -> do
    let encryptionKey :: Maybe Value -- Value in order to avoid parsing the key, which is expensive.
        encryptionKey = parseMaybe (withObject "AccountInfo" (.: "accountEncryptionKey")) accInfo
    case encryptionKey of
      Nothing -> do
        $(logInfo) $ "Account not found for 'accountEncryptionKey' request: " <> addrText
        respond404Error EMAccountDoesNotExist
      Just key -> do
        $(logInfo) $ "Retrieved account encryption key for " <> addrText
                <> ": " <> Text.pack (show encryptionKey)
        sendResponse (object [ "accountEncryptionKey" .= key ])

  where doGetEncryptionKey = withBestBlockHash Nothing (getAccountInfo addrText)


-- |Get the cost of a transaction, based on its type. The transaction type is
-- given as a "type" parameter. An additional parameter is "numSignatures" that
-- defaults to one if not present.
--
-- TODO: This currently assumes a conversion factor of 100 from
-- energy to GTU.
getTransactionCostR :: Handler TypedContent
getTransactionCostR = withExchangeRate $ \rate -> do
  numSignatures <- fromMaybe "1" <$> lookupGetParam "numSignatures"
  case readMaybe (Text.unpack numSignatures) of
    Just x | x > 0 -> handleTransactionCost rate x
    _ -> respond400Error (EMParseError "Could not parse `numSignatures` value.") RequestInvalid

  where
      handleTransactionCost rate numSignatures = do
          lookupGetParam "type" >>= \case
            Nothing -> respond400Error EMMissingParameter RequestInvalid
            Just tty -> case Text.unpack tty of
              "simpleTransfer" -> do
                let energyCost = simpleTransferEnergyCost simpleTransferPayloadSize numSignatures
                sendResponse $ object ["cost" .= computeCost rate energyCost
                                      , "energy" .= energyCost
                                      ]
              y | y == "encryptedTransfer" -> do
                let energyCost = encryptedTransferEnergyCost encryptedTransferPayloadSize numSignatures
                sendResponse $ object ["cost" .= computeCost rate energyCost
                                      , "energy" .= energyCost
                                      ]
                | y == "transferToSecret" -> do
                    let energyCost = accountEncryptEnergyCost accountEncryptPayloadSize numSignatures
                    sendResponse $ object ["cost" .= computeCost rate energyCost
                                          , "energy" .= energyCost
                                          ]
                | y == "transferToPublic" -> do
                    let energyCost = accountDecryptEnergyCost accountDecryptPayloadSize numSignatures
                    sendResponse $ object ["cost" .= computeCost rate energyCost
                                          , "energy" .= energyCost
                                          ]
              tty' -> respond400Error (EMParseError $ "Could not parse transaction type: " <> tty') RequestInvalid
      fetchUpdates :: ClientMonad IO (Either String EnergyRate)
      fetchUpdates = do
          bbh <- getBestBlockHash
          summary <- getBlockSummary bbh
          -- This extraction of the parameter is not ideal for two reasons
          -- - this is the exchange rate in the best block, which could already be obsolete.
          --   This is not likely not matter since block times 10s on average, and it is always the case
          --   that the transaction is committed after the current time. In any case this is only an estimate.
          -- - It is manually parsing the return value, instead of using the Updates type. This should be fixed
          --   and we want to use the same calculation in concordium-client, however that requires more restructuring
          --   of the dependencies. The current solution is good enough in the meantime.
          let energyRateParser = AE.withObject "Block summary" $ \obj -> do
                updates <- obj AE..: "updates"
                chainParameters <- updates AE..: "chainParameters"
                euroPerEnergy <- chainParameters AE..: "euroPerEnergy"
                microGTUPerEuro <- chainParameters AE..: "microGTUPerEuro"
                return $ computeEnergyRate microGTUPerEuro euroPerEnergy
          return $ parseEither energyRateParser =<< summary
      withExchangeRate = runGRPC fetchUpdates
putCredentialR :: Handler TypedContent
putCredentialR =
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success Versioned{..} | vVersion == 0 -> do
          runGRPC (sendTransactionToBaker (CredentialDeployment vValue) defaultNetId) $ \case
            False -> do -- this happens if the request is duplicate, stale, or malformed.
              $(logError) "Credential rejected by node."
              respond400Error EMCredentialRejected RequestInvalid
            True ->
              sendResponse (object ["submissionId" .= (getHash (CredentialDeployment vValue) :: TransactionHash)])
                              | otherwise -> respond400Error (EMParseError $ "Invalid version number " ++ show vVersion) RequestInvalid

-- |Use the serialize instance of a type to deserialize
decodeBase16 :: (MonadFail m) => Text.Text -> m BS.ByteString
decodeBase16 t =
    if BS.null rest then return bs
    else fail $ "Could not decode as base-16: " ++ show t
    where
        (bs, rest) = BS16.decode (Text.encodeUtf8 t)


putTransferR :: Handler TypedContent
putTransferR =
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right txJSON ->
      case parse transferParser txJSON  of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success tx -> do
          $(logInfo) (Text.pack (show tx))
          runGRPC (sendTransactionToBaker (NormalTransaction tx) defaultNetId) $ \case
            False -> do -- transaction invalid
              $(logError) "Transaction rejected by the node."
              respond400Error EMTransactionRejected RequestInvalid
            True ->
              sendResponse (object ["submissionId" .= (getHash (NormalTransaction tx) :: TransactionHash)])
      where transferParser = withObject "Parse transfer request." $ \obj -> do
              sig :: TransactionSignature <- obj .: "signatures"
              body <- decodeBase16 =<< (obj .: "transaction")
              case S.decode (S.encode sig <> body) of
                Left err -> fail err
                Right tx -> return tx

getSimpleTransactionStatus :: MonadIO m => I18n -> TransactionHash -> ClientMonad m (Either String Value)
getSimpleTransactionStatus i trHash = do
    eitherStatus <- getTransactionStatus (Text.pack $ show trHash)
    return $
      eitherStatus >>= \case
        Null -> return $ object ["status" .= String "absent"]
        Object o -> do
          parseEither (.: "status") o >>= \case
            "received" -> return $ object ["status" .= String "received"]
            "finalized" -> HM.toList <$> parseEither (.: "outcomes") o >>= \case
              [(bh,outcome)] -> do
                fields <- outcomeToPairs outcome
                return $ object $ ["status" .= String "finalized", "blockHashes" .= [bh :: BlockHash]] <> fields
              _ -> throwError "expected exactly one outcome for a finalized transaction"
            "committed" -> do
              outcomes <- HM.toList <$> parseEither (.: "outcomes") o
              fields <- outcomesToPairs (snd <$> outcomes)
              return $ object $ ["status" .= String "committed", "blockHashes" .= (fst <$> outcomes :: [BlockHash])] <> fields
            s -> throwError ("unexpected \"status\": " <> s)
        _ -> throwError "expected null or object"
  where
    outcomeToPairs :: TransactionSummary -> Either String [Pair]
    outcomeToPairs TransactionSummary{..} =
      (["transactionHash" .= tsHash
      , "sender" .= tsSender
      , "cost" .= tsCost] <>) <$>
      case tsType of
        TSTCredentialDeploymentTransaction _ -> -- credential deployment
          case tsResult of
            TxSuccess [AccountCreated {}, _] ->
              return ["outcome" .= String "success"]
            TxSuccess [CredentialDeployed {}] ->
              return ["outcome" .= String "success"]
            es ->
              Left $ "Unexpected outcome of credential deployment: " ++ show es
        TSTAccountTransaction (Just TTTransfer) ->
          case tsResult of
            TxSuccess [Transferred{etTo = AddressAccount addr,..}] ->
              return ["outcome" .= String "success",
                      "to" .= addr,
                      "amount" .= etAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of simple transfer: " ++ show es
        TSTAccountTransaction (Just TTEncryptedAmountTransfer) ->
          case tsResult of
            TxSuccess [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}] ->
              return ["outcome" .= String "success",
                      "sender" .= earAccount,
                      "to" .= neaAccount,
                      "encryptedAmount" .= neaEncryptedAmount,
                      "inputEncryptedAmount" .= earInputAmount,
                      "aggregatedIndex" .= earUpToIndex,
                      "newSelfEncryptedAmount" .= earNewAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of encrypted transfer: " ++ show es
        TSTAccountTransaction (Just TTTransferToPublic) ->
          case tsResult of
            TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}] ->
              return ["outcome" .= String "success",
                      "sender" .= earAccount,
                      "newSelfEncryptedAmount" .= earNewAmount,
                      "inputEncryptedAmount" .= earInputAmount,
                      "aggregatedIndex" .= earUpToIndex,
                      "amountAdded" .= aabdAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of secret to public transfer: " ++ show es
        TSTAccountTransaction (Just TTTransferToEncrypted) ->
          case tsResult of
            TxSuccess [EncryptedSelfAmountAdded{..}] ->
              return ["outcome" .= String "success",
                      "sender" .= eaaAccount,
                      "newSelfEncryptedAmount" .= eaaNewAmount,
                      "amountSubtracted" .= eaaAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of public to secret transfer: " ++ show es
        _ ->
          Left "Unsupported transaction type for simple statuses."
    outcomesToPairs :: [TransactionSummary] -> Either String [Pair]
    outcomesToPairs l = do
      outcomes <- mapM outcomeToPairs l
      case outcomes of
        [] -> Left "Expected at least one transaction outcome for a committed transaction"
        [o] -> return o
        (h:r)
          | all (h==) r -> return h
          | otherwise -> return ["outcome" .= String "ambiguous"]

-- Get the status of the submission.
getSubmissionStatusR :: Text -> Handler TypedContent
getSubmissionStatusR submissionId =
  case readMaybe (Text.unpack submissionId) of
    Nothing -> respond400Error EMMalformedTransaction RequestInvalid
    Just txHash -> do
      i <- internationalize
      runGRPC (getSimpleTransactionStatus i txHash) (sendResponse . toJSON)

getAccountTransactionsR :: Text -> Handler TypedContent
getAccountTransactionsR addrText = do
  i <- internationalize
  case addressFromText addrText of
    Left _ -> respond400Error EMMalformedAddress RequestInvalid
    Right addr -> do
      order <- lookupGetParam "order"
      let (ordering, ordType :: Text, ordRel) = case order of
                        Just (Text.unpack . Text.toLower -> ('d':_)) -> (E.desc, "descending", (E.<.))
                        _ -> (E.asc, "ascending", (E.>.))
      startId :: Maybe EntryId <- (>>= fromPathPiece) <$> lookupGetParam "from"
      limit <- maybe 20 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"
      -- Construct a "transaction type filter" to only query the relevant transaction types specified by `includeRewards`.
      -- This is done as part of the SQL query since it is both more efficient, but also simpler since we do not have to filter
      -- on the client side.
      -- In this typefilter we make use of the `veryUnsafeCoerceSqlExprValue` which we really do not need,
      -- but I cannot find any API in Esqueleto that would allow us to transform from AE.Value to EJ.JSONB Value
      -- even though this should be possible.
      -- This function should either be fixed to use the Persistent abstractions without Esqueleto, the database schema type
      -- should be changed to use JSONB, or the relevant compatibility function should be added to Esqueleto.
      -- Because we are pressed for time I have the solution at the moment.
      maybeTypeFilter <- lookupGetParam "includeRewards" <&> \case
        Nothing -> Just $ const (return ()) -- the default
        Just "all" -> Just $ const (return ())
        Just "allButFinalization" -> Just $ \s ->
          -- check if
          -- - either the transaction is an account transaction
          -- - or if not check that it is not a finalization reward.
          let coerced = E.just (EInternal.veryUnsafeCoerceSqlExprValue (s E.^. SummarySummary))
              isAccountTransaction = coerced EJ.?. "Left"
              extractedTag = coerced EJ.#>>. ["Right", "tag"]
          in E.where_ (isAccountTransaction E.||. extractedTag E.!=. E.val (Just "FinalizationRewards"))
        Just "none" -> Just $ \s -> E.where_ (E.just (EInternal.veryUnsafeCoerceSqlExprValue (s E.^. SummarySummary)) EJ.?. "Left") -- Left are account transactions.
        Just _ -> Nothing
      rawReason <- isJust <$> lookupGetParam "includeRawRejectReason"
      case maybeTypeFilter of
        Nothing -> respond400Error (EMParseError "Unsupported 'includeRewards' parameter.") RequestInvalid
        Just typeFilter -> do
          entries :: [(Entity Entry, Entity Summary)] <- runDB $ do
            E.select $ E.from $ \(e, s) ->  do
              -- Assert join
              E.where_ (e E.^. EntrySummary E.==. s E.^. SummaryId)
              -- Filter by address
              E.where_ (e E.^. EntryAccount E.==. E.val (ByteStringSerialized addr))
              -- If specified, start from the given starting id
              maybe
                (return ())
                (\sid -> E.where_ (e E.^. EntryId `ordRel` E.val sid))
                startId
              typeFilter s
              -- sort with the requested method or ascending over EntryId.
              E.orderBy [ordering (e E.^. EntryId)]
              -- Limit the number of returned rows
              E.limit limit
              return (e, s)
          case mapM (formatEntry rawReason i addr) entries of
            Left err -> do
              $(logError) $ "Error decoding transaction: " <> Text.pack err
              sendResponseStatus badGateway502 $ object [
                "errorMessage" .= i18n i EMDatabaseError,
                "error" .= fromEnum InternalError
                ]
            Right fentries -> sendResponse $ object $ [
              "limit" .= limit,
              "order" .= ordType,
              "count" .= length fentries,
              "transactions" .= fentries] <>
              (maybe [] (\sid -> ["from" .= sid]) startId)

-- |Convert a timestamp to seconds since the unix epoch. A timestamp can be a fractional number, e.g., 17.5.
timestampToFracSeconds :: Timestamp -> Double
timestampToFracSeconds Timestamp{..} = fromRational (toInteger tsMillis Rational.% 1000)

-- |Format a transaction affecting an account.
formatEntry :: Bool -- ^ Whether to include a raw reject reason for account transactions or not.
            -> I18n -- ^ Internationalization of messages.
            -> AccountAddress -- ^ Address of the account whose transactions we are formatting.
            -> (Entity Entry, Entity Summary) -- ^ Database entry to be formatted.
            -> Either String Value
formatEntry rawRejectReason i self (Entity key Entry{}, Entity _ Summary{..}) = do
  let blockDetails = ["blockHash" .= unBSS summaryBlock,
                      "blockTime" .= timestampToFracSeconds summaryTimestamp
                     ]
  transactionDetails <- case AE.fromJSON summarySummary of
    AE.Error e -> Left e
    AE.Success (Right v@BakingRewards{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= Map.lookup self (accountAmounts stoBakerRewards), -- this should always return Just due to the way we look up.
        "details" .= object [
          "type" .= ("bakingReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
          ]
      ]
    AE.Success (Right v@Mint{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= stoMintPlatformDevelopmentCharge, -- this will only happen if we are looking up the foundation account
        "details" .= object [
          "type" .= ("platformDevelopmentCharge" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
          ]
      ]
    AE.Success (Right v@FinalizationRewards{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= Map.lookup self (accountAmounts stoFinalizationRewards), -- this should always return Just due to the way we look up.
        "details" .= object [
          "type" .= ("finalizationReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
          ]
      ]
    AE.Success (Right v@BlockReward{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= if self == stoBaker && self == stoFoundationAccount then stoBakerReward + stoFoundationCharge
                   else if self == stoBaker then stoBakerReward
                   else stoFoundationCharge, -- due to the way we index, that is the only remaining option
        "details" .= object [
          "type" .= ("blockReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
          ]
      ]
    AE.Success (Left TransactionSummary{..}) -> do
      let (origin, selfOrigin) = case tsSender of
                                   Just sender
                                     | sender == self -> (object ["type" .= ("self" :: Text)], True)
                                     | otherwise -> (object ["type" .= ("account" :: Text), "address" .= sender], False)
                                   Nothing -> (object ["type" .= ("none" :: Text)], False)

          (resultDetails, subtotal) = case tsResult of
            TxSuccess evts -> ((["outcome" .= ("success" :: Text), "events" .= fmap (i18n i) evts]
                                <> case (tsType, evts) of
                                     (TSTAccountTransaction (Just TTTransfer), [Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr)]) ->
                                       ["transferSource" .= fromAddr,
                                        "transferDestination" .= toAddr,
                                        "transferAmount" .= amt]
                                     (TSTAccountTransaction (Just TTEncryptedAmountTransfer), [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]) ->
                                       ["transferSource" .= earAccount,
                                        "transferDestination" .= neaAccount,
                                        "encryptedAmount" .= neaEncryptedAmount,
                                        "aggregatedIndex" .= earUpToIndex,
                                        "inputEncryptedAmount" .= earInputAmount,
                                        "newIndex" .= neaNewIndex,
                                        "newSelfEncryptedAmount" .= earNewAmount]
                                     (TSTAccountTransaction (Just TTTransferToPublic), [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}]) ->
                                       ["transferSource" .= earAccount,
                                        "amountAdded" .= aabdAmount,
                                        "aggregatedIndex" .= earUpToIndex,
                                        "inputEncryptedAmount" .= earInputAmount,
                                        "newSelfEncryptedAmount" .= earNewAmount]
                                     (TSTAccountTransaction (Just TTTransferToEncrypted), [EncryptedSelfAmountAdded{..}]) ->
                                       ["transferSource" .= eaaAccount,
                                        "amountSubtracted" .= eaaAmount,
                                        "newSelfEncryptedAmount" .= eaaNewAmount]
                                     (TSTAccountTransaction (Just TTTransferWithSchedule), [TransferredWithSchedule{..}]) ->
                                       ["transferDestination" .= etwsTo,
                                        "transferAmount" .= foldl' (+) 0 (map snd etwsAmount)]

                                     _ -> []), eventSubtotal self evts )
            TxReject reason ->
              let rawReason = if rawRejectReason then ["rawRejectReason" .= reason] else []
              in (["outcome" .= ("reject" :: Text), "rejectReason" .= i18n i reason] ++ rawReason, Nothing)

          details = object $ ["type" .= renderTransactionSummaryType tsType, "description" .= i18n i tsType] <> resultDetails

          costs
            | selfOrigin = case subtotal of
                Nothing -> let total = - toInteger tsCost in ["cost" .= show (toInteger tsCost), "total" .= show total]
                Just st -> let total = st - toInteger tsCost
                          in ["subtotal" .= show st, "cost" .= show (toInteger tsCost), "total" .= show total]
            | otherwise = ["total" .= show (fromMaybe 0 subtotal)]

          encryptedCost = case tsSender of
            Just sender
              | sender == self -> case (tsType, tsResult) of
                  (TSTAccountTransaction (Just TTEncryptedAmountTransfer), TxSuccess [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]) ->
                    ["encrypted" .= object ["encryptedAmount" .= neaEncryptedAmount,
                                            "newStartIndex" .= earUpToIndex,
                                            "newSelfEncryptedAmount" .= earNewAmount]]
                  (TSTAccountTransaction (Just TTTransferToPublic), TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{}]) ->
                    ["encrypted" .= object ["newStartIndex" .= earUpToIndex,
                                            "newSelfEncryptedAmount" .= earNewAmount]]
                  (TSTAccountTransaction (Just TTTransferToEncrypted), TxSuccess [EncryptedSelfAmountAdded{..}]) ->
                    ["encrypted" .= object ["newSelfEncryptedAmount" .= eaaNewAmount]]
                  _ -> []
              | otherwise -> case (tsType, tsResult) of
                  (TSTAccountTransaction (Just TTEncryptedAmountTransfer), TxSuccess [EncryptedAmountsRemoved{}, NewEncryptedAmount{..}]) ->
                    ["encrypted" .= object ["encryptedAmount" .= neaEncryptedAmount,
                                            "newIndex" .= neaNewIndex]]
                  _ -> []

            Nothing -> []
      return $ [
          "origin" .= origin,
          "energy" .= tsEnergyCost,
          "details" .= details,
          "transactionHash" .= tsHash
          ] <> costs <> encryptedCost
  return $ object $ ["id" .= key] <> blockDetails <> transactionDetails

renderTransactionType :: TransactionType -> Text
renderTransactionType TTDeployModule = "deployModule"
renderTransactionType TTInitContract = "initContract"
renderTransactionType TTUpdate = "update"
renderTransactionType TTUpdateBakerStake = "updateBakerStake"
renderTransactionType TTUpdateBakerKeys = "updateBakerKeys"
renderTransactionType TTUpdateBakerRestakeEarnings = "updateBakerRestakeEarnings"
renderTransactionType TTTransfer = "transfer"
renderTransactionType TTAddBaker = "addBaker"
renderTransactionType TTRemoveBaker = "removeBaker"
renderTransactionType TTUpdateCredentialKeys = "updateAccountKeys"
renderTransactionType TTEncryptedAmountTransfer = "encryptedAmountTransfer"
renderTransactionType TTTransferToEncrypted = "transferToEncrypted"
renderTransactionType TTTransferToPublic = "transferToPublic"
renderTransactionType TTTransferWithSchedule = "transferWithSchedule"
renderTransactionType TTUpdateCredentials = "updateCredentials"
renderTransactionType TTRegisterData = "registerData"

renderTransactionSummaryType :: TransactionSummaryType -> Text
renderTransactionSummaryType (TSTAccountTransaction (Just tt)) = renderTransactionType tt
renderTransactionSummaryType (TSTAccountTransaction Nothing) = "Malformed account transaction"
renderTransactionSummaryType (TSTCredentialDeploymentTransaction _) = "deployCredential"
renderTransactionSummaryType (TSTUpdateTransaction _) = "chainUpdate"

eventSubtotal :: AccountAddress -> [Event] -> Maybe Integer
eventSubtotal self evts = case catMaybes $ eventCost <$> evts of
    [] -> Nothing
    l -> Just (sum l)
  where
    isSelf (AddressAccount acc) = self == acc
    isSelf _ = False
    eventCost ContractInitialized{..} = Just (- toInteger ecAmount)
    eventCost Updated{..}
      | isSelf euInstigator = Just (- toInteger euAmount)
      | otherwise = Nothing
    eventCost Transferred{..} = case (isSelf etFrom, isSelf etTo) of
      (True, True) -> Just 0
      (True, False) -> Just (- toInteger etAmount)
      (False, True) -> Just (toInteger etAmount)
      (False, False) -> Nothing
    eventCost TransferredWithSchedule{..} =
      if self == etwsFrom then -- self transfers are not possible with schedule
        Just (- toInteger (sum . map snd $ etwsAmount))
      else Just (toInteger (sum . map snd $ etwsAmount))
    eventCost AmountAddedByDecryption{..} = Just $ toInteger aabdAmount
    eventCost EncryptedSelfAmountAdded{..} = Just $ - toInteger eaaAmount
    eventCost _ = Nothing

dropAmount :: Amount
dropAmount = 2000000000


{- | Try to execute a GTU drop to the given account.

1.  Lookup the account on the chain
        - If it's not finalized, return 404 Not Found (account not final)
2.  Lookup the account in the database
    A. If there is an entry,
        A.1. query the send account's last finalized nonce and balance
          - on failure, return 502 Bad Gateway (configuration error)
        A.2. determine the state of the transaction
        - received, committed, or successfully finalized: report transaction hash
        - absent, or unsuccessfully finalized:
          - if the GTU drop account has insufficient funds, return 502 Bad Gateway (configuration error)
          - if the transaction nonce is finalized or the transaction is expired, delete the entry and trety from 2
          - otherwise, send the transaction to the node and report the transaction hash
    B. If there is no entry
        B.1. query the sender's next available nonce
        B.2. produce/sign the transaction
        B.3. store the transaction in the database
            - on failure, retry from 2
        B.4. submit the transaction and report transaction hash
-}

data DropSchedule = Scheduled | Normal

instance FromJSON DropSchedule where
  parseJSON = withObject "Drop schedule" $ \obj -> do
    ty <- obj .: "type"
    if ty == Text.pack "scheduled" then
      return Scheduled
    else if ty == Text.pack "normal" then
      return Normal
    else fail "Unsupported drop type."

-- | Handle a GTU drop request.
--     When 'gtuDropData' is provided: handle drop.
--     Otherwise: return 404.
putGTUDropR :: Text -> Handler TypedContent
putGTUDropR addrText = do
    i <- internationalize
    Proxy{..} <- getYesod
    case gtuDropData of
      Nothing -> respond404Error $ EMErrorResponse NotFound
      Just gtuDropData' -> do
        case addressFromText addrText of
          Left _ -> respond400Error EMMalformedAddress RequestInvalid
          Right addr -> runGRPC (doGetAccInfo addrText) $ \case
              -- Account is not finalized
              Nothing -> sendResponseStatus notFound404 $ object
                          ["errorMessage" .= i18n i EMAccountNotFinal,
                          "error" .= fromEnum RequestInvalid]
              -- Account is finalized, so try the drop
              Just _ -> do
                connect rawRequestBody (sinkParserEither json') >>= \case
                  Left _ -> tryDrop addr Normal gtuDropData' -- malformed or empty body, we assume normal drop.
                  Right v -> case fromJSON v of
                              AE.Success x -> tryDrop addr x gtuDropData'
                              AE.Error e -> do
                                $(logWarn) (Text.pack e)
                                respond400Error EMConfigurationError RequestInvalid

  where
    accountToText = Text.decodeUtf8 . addressToBytes
    doGetAccInfo :: Text -> ClientMonad IO (Either String (Maybe (Nonce, Amount)))
    doGetAccInfo t = do
      lastFinBlock <- getLastFinalBlockHash
      ai <- getAccountInfo t lastFinBlock
      case ai of
        Right Null -> return $ Right Nothing
        Right val -> return $ Just <$> parseEither (withObject "account info" $ \o -> (,) <$> (o .: "accountNonce") <*> (o .: "accountAmount")) val
        Left err -> return $ Left err
    -- Determine if the transaction is or could become
    -- successfully finalized.  Returns False if the
    -- transaction is absent or is finalized but failed.
    doIsTransactionOK trHash = do
      eitherStatus <- getTransactionStatus (Text.pack $ show trHash)
      return $
        eitherStatus >>= \case
          Null -> return False
          Object o -> parseEither (.: "status") o >>= \case
            "received" -> return True
            "finalized" -> HM.toList <$> parseEither (.: "outcomes") o >>= \case
              [(_::BlockHash,TransactionSummary{..})] ->
                case tsResult of
                  TxSuccess{} -> return True
                  TxReject{} -> return False
              _ -> throwError "expected exactly one outcome for a finalized transaction"
            "committed" -> return True
            s -> throwError ("unexpected \"status\": " <> s)
          _ -> throwError "expected null or object"
    sendTransaction transaction
      = runGRPC (sendTransactionToBaker (NormalTransaction transaction) defaultNetId) $ \case
          False -> do -- this case cannot happen at this time
            $(logError) "GTU drop transaction rejected by node."
            respond400Error EMConfigurationError RequestInvalid
          True ->
            sendResponse (object ["submissionId" .= (getHash (NormalTransaction transaction) :: TransactionHash)])
    configErr = do
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMConfigurationError,
        "error" .= fromEnum InternalError
        ]
    tryDrop addr dropType gtuDropData@GTUDropData{..} = do
      -- number of keys we need to sign with
      let numKeys = sum . map (length . snd) $ dropKeys
      let getNonce = (>>= parseEither (withObject "nonce" (.: "nonce"))) <$> getNextAccountNonce (accountToText dropAccount)
      -- Determine if there is already a GTU drop entry for this account
      rcpRecord <- runDB $ getBy (UniqueAccount (ByteStringSerialized addr))
      case rcpRecord of
        -- If there is no entry, try the drop
        Nothing -> runGRPC getNonce $ \nonce -> do
          currentTime <- liftIO $ round <$> getPOSIXTime
          (payload, thEnergyAmount) <- case dropType of
            Normal -> return (Transfer addr dropAmount, simpleTransferEnergyCost simpleTransferPayloadSize numKeys)
            Scheduled -> do
              -- sample a random release schedule spaced by 5min.
              numRels <- liftIO $ randomRIO (1::Word,15)
              let start = Timestamp {tsMillis = currentTime * 1000 + 300 * 1000}
                  (releaseAmount, remainder) = (fromIntegral dropAmount :: Word) `divMod` fromIntegral numRels
                  releases = [(addDuration start (fromIntegral i * 300 * 1000),
                               if i == 1
                               then fromIntegral (remainder + releaseAmount)
                               else fromIntegral releaseAmount) | i <- [1 .. numRels]]
              return (TransferWithSchedule addr releases, transferWithScheduleEnergyCost (transferWithSchedulePayloadSize (fromIntegral numRels)) (fromIntegral numRels) numKeys)
          let
            atrPayload = encodePayload payload
            atrHeader = TransactionHeader {
              thSender = dropAccount,
              thNonce = nonce,
              thPayloadSize = payloadSize atrPayload,
              thExpiry = TransactionTime $ currentTime + 300,
              ..
            }
            transaction = signTransaction dropKeys atrHeader atrPayload
          mk <- runDB $ insertUnique (GTURecipient (ByteStringSerialized addr) (ByteStringSerialized transaction))
          case mk of
            -- There is already a transaction, so retry.
            Nothing -> tryDrop addr dropType gtuDropData
            Just _ -> sendTransaction transaction
        Just (Entity key (GTURecipient _ (ByteStringSerialized transaction))) ->
          runGRPC (doGetAccInfo (accountToText dropAccount)) $ \case
              Nothing -> do
                $(logError) $ "Could not get GTU drop account ('" <> accountToText dropAccount  <> "') info."
                configErr
              Just (lastFinNonce, lastFinAmt) -> do
                let trHash = getHash (NormalTransaction transaction) :: TransactionHash
                runGRPC (doIsTransactionOK trHash) $ \case
                  True -> sendResponse (object ["submissionId" .= trHash])
                  False
                    | lastFinAmt < dropAmount + 1000000 -- FIXME: This is outdated.
                      -> do
                        $(logError) "GTU drop account has insufficient funds"
                        configErr
                    | lastFinNonce > thNonce (atrHeader transaction)
                      -> do
                        -- Given the nonce, the transaction is no good. Delete and try again.
                        runDB $ delete key
                        tryDrop addr dropType gtuDropData
                    | otherwise
                      -> do
                        currentTime <- liftIO $ round <$> getPOSIXTime
                        if thExpiry (atrHeader transaction) < TransactionTime currentTime then do
                          runDB $ delete key
                          tryDrop addr dropType gtuDropData
                        else sendTransaction transaction

getGlobalFileR :: Handler TypedContent
getGlobalFileR = toTypedContent . globalInfo <$> getYesod

-- Queries the transaction database and the GRPC, 
-- then if both succeed checks that the last final block is less than 5 minutes old.
getHealthR :: Handler TypedContent
getHealthR = 
  runGRPC doGetBlockFinalInfo $ \case
      Nothing -> do
        $(logError) $ "Could not get response from GRPC."
        sendResponse $ object $ ["healthy" .= False, "reason" .= ("Could not get response from GRPC.":: String)]
      Just lastFinalBlockInfo -> do
        $(logInfo) "Successfully got best block info."
        result :: [(Entity Entry, Entity Summary)] <- runDB $ E.select $
                  E.from $ \val -> do
                  E.limit 1 -- query for any single row
                  return val
        case result of
          [] ->
            sendResponse $ object $ ["healthy" .= False, "reason" .= ("Could not get response from database.":: String)]
          _ -> do
            $(logInfo) "Successfully queried database."
            -- get block slot time from block info object, compare with current time: reject if more than 5 minutes:
            case lastFinalBlockInfo of
              Object hm -> 
                case HM.lookup "blockSlotTime" hm of
                  Nothing -> sendResponse $ object $ ["healthy" .= False, "reason" .= ("Block info format has changed. blockSlotTime field missing.":: String)]
                  Just time -> 
                    case fromJSON time of
                      Error _ -> sendResponse $ object $ ["healthy" .= False, "reason" .= ("Block info format has changed. blockSlotTime is not UTCTime.":: String)]
                      Success (lastFinalBlockTime :: Clock.UTCTime) -> do 
                        currentTime <- liftIO Clock.getCurrentTime
                        case (Clock.diffUTCTime currentTime lastFinalBlockTime) < (Clock.secondsToNominalDiffTime 300) of -- 5 minutes = 300 seconds
                          True -> sendResponse $ object $ ["healthy" .= True, "lastFinalTime" .= lastFinalBlockTime]
                          False -> sendResponse $ object $ ["healthy" .= False, "reason" .= ("The last final block is too old.":: String)] 
              _ -> sendResponse $ object $ ["healthy" .= False, "reason" .= ("Block info format has changed. Not an object.":: String)]
  where 
    doGetBlockFinalInfo :: ClientMonad IO (Either String (Maybe Value))
    doGetBlockFinalInfo = do
      bbi <- withLastFinalBlockHash Nothing getBlockInfo
      case bbi of
        Right Null -> return $ Right Nothing
        Right val -> return $ Right $ Just val
        Left err -> return $ Left err

getIpsR :: Handler TypedContent
getIpsR = toTypedContent . ipInfo <$> getYesod
