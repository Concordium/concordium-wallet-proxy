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
import Text.Read hiding (String)
import Control.Arrow (left)
import Control.Monad.Except
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
import Data.Maybe(catMaybes, fromMaybe)
import Data.Time.Clock.POSIX
import qualified Database.Esqueleto as E

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Execution
import Concordium.Client.GRPC
import Concordium.Client.Types.Transaction (simpleTransferEnergyCost, encryptedTransferEnergyCost)
import Concordium.ID.Types (addressFromText, addressToBytes, KeyIndex)
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.Common.Version
import Concordium.GlobalState.SQL.AccountTransactionIndex
import Concordium.GlobalState.SQL

import Internationalization

data ErrorCode = InternalError | RequestInvalid | DataNotFound
    deriving(Eq, Show, Enum)

data Proxy = Proxy {
  grpcEnvData :: !EnvData,
  dbConnectionPool :: ConnectionPool,
  dropAccount :: AccountAddress,
  dropKeys :: [(KeyIndex, KeyPair)],
  globalInfo :: Value,
  ipInfo :: Value
}
instance Yesod Proxy where
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
        transaction (ByteStringSerialized BareTransaction)
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
            return $ object ["accountAmount" .= publicAmount, "accountEncryptedAmount" .= encryptedAmount]
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
getTransactionCostR = do
  numSignatures <- fromMaybe "1" <$> lookupGetParam "numSignatures"
  case readMaybe (Text.unpack numSignatures) of
    Just x | x > 0 -> handleTransactionCost x
    _ -> respond400Error (EMParseError "Could not parse `numSignatures` value.") RequestInvalid

  where handleTransactionCost numSignatures = do
          lookupGetParam "type" >>= \case
            Nothing -> respond400Error EMMissingParameter RequestInvalid
            Just tty -> case Text.unpack tty of
              "simpleTransfer" -> sendResponse $ object ["cost" .= Amount (100 * fromIntegral (simpleTransferEnergyCost numSignatures))
                                                       , "energy" .= simpleTransferEnergyCost numSignatures
                                                       ]
              y | y == "encryptedTransfer" ||
                  y == "transferToSecret" ||
                  y == "transferToPublic" -> do
                -- FIXME: Dummy values for a prototype
                let dummyCost = encryptedTransferEnergyCost numSignatures -- roughly 30ms of energy
                sendResponse $ object ["cost" .= Amount (100 * fromIntegral dummyCost)
                                      , "energy" .= dummyCost
                                      ]
              tty' -> respond400Error (EMParseError $ "Could not parse transaction type: " <> tty') RequestInvalid

putCredentialR :: Handler TypedContent
putCredentialR =
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success Versioned{..} | vVersion == 0 -> do
          runGRPC (sendTransactionToBaker (CredentialDeployment vValue) defaultNetId) $ \case
            False -> do -- this case cannot happen at this time
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
            False -> do -- this case cannot happen at this time
              $(logError) "Credential rejected by node."
              respond400Error EMCredentialRejected RequestInvalid
            True ->
              sendResponse (object ["submissionId" .= (getHash (NormalTransaction tx) :: TransactionHash)])
      where transferParser = withObject "Parse transfer request." $ \obj -> do
              sig :: TransactionSignature <- obj .: "signatures"
              body <- decodeBase16 =<< (obj .: "transaction")
              case S.decode ((S.encode sig) <> body) of
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
        Nothing -> -- credential deployment
          case tsResult of
            TxSuccess [AccountCreated {}, _] ->
              return ["outcome" .= String "success"]
            TxSuccess [CredentialDeployed {}] ->
              return ["outcome" .= String "success"]
            es ->
              Left $ "Unexpected outcome of credential deployment: " ++ show es
        Just TTTransfer ->
          case tsResult of
            TxSuccess [Transferred{etTo = AddressAccount addr,..}] ->
              return ["outcome" .= String "success",
                      "to" .= addr,
                      "amount" .= etAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of simple transfer: " ++ show es
        Just TTEncryptedAmountTransfer ->
          case tsResult of
            TxSuccess [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}] ->
              return ["outcome" .= String "success",
                      "sender" .= earAccount,
                      "to" .= neaAccount,
                      "encryptedAmount" .= neaEncryptedAmount,
                      "aggregatedIndex" .= earUpToIndex,
                      "newSelfEncryptedAmount" .= earNewAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of encrypted transfer: " ++ show es
        Just TTTransferToPublic ->
          case tsResult of
            TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}] ->
              return ["outcome" .= String "success",
                      "sender" .= earAccount,
                      "newSelfEncryptedAmount" .= earNewAmount,
                      "aggregatedIndex" .= earUpToIndex,
                      "amountAdded" .= aabdAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of secret to public transfer: " ++ show es
        Just TTTransferToEncrypted ->
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
          -- sort with the requested method or ascending over EntryId.
          E.orderBy [ordering (e E.^. EntryId)]
          -- Limit the number of returned rows
          E.limit limit
          return (e, s)
      case mapM (formatEntry i addr) entries of
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

formatEntry :: I18n -> AccountAddress -> (Entity Entry, Entity Summary) -> Either String Value
formatEntry i self (Entity key Entry{..}, Entity _ Summary{..}) = do
  let blockDetails = ["blockHash" .= unBSS summaryBlock,
                      "blockTime" .= timestampToFracSeconds summaryTimestamp
                     ]
  transactionDetails <- case AE.fromJSON summarySummary of
    AE.Error e -> Left e
    AE.Success (Right v@BakingReward{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= stoRewardAmount,
        "details" .= object [
          "type" .= ("bakingReward" :: Text),
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
                                     (Just TTTransfer, [Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr)]) ->
                                       ["transferSource" .= fromAddr,
                                        "transferDestination" .= toAddr,
                                        "transferAmount" .= amt]
                                     (Just TTEncryptedAmountTransfer, [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]) ->
                                       ["transferSource" .= earAccount,
                                        "transferDestination" .= neaAccount,
                                        "encryptedAmount" .= neaEncryptedAmount,
                                        "aggregatedIndex" .= earUpToIndex,
                                        "newIndex" .= neaNewIndex,
                                        "newSelfEncryptedAmount" .= earNewAmount]
                                     (Just TTTransferToPublic, [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}]) ->
                                       ["transferSource" .= earAccount,
                                        "amountAdded" .= aabdAmount,
                                        "aggregatedIndex" .= earUpToIndex,
                                        "newSelfEncryptedAmount" .= earNewAmount]
                                     (Just TTTransferToEncrypted, [EncryptedSelfAmountAdded{..}]) ->
                                       ["transferSource" .= eaaAccount,
                                        "amountSubtracted" .= eaaAmount,
                                        "newSelfEncryptedAmount" .= eaaNewAmount]
                                     _ -> []), eventSubtotal self evts )
            TxReject reason -> (["outcome" .= ("reject" :: Text), "rejectReason" .= i18n i reason], Nothing)

          details = object $ ["type" .= renderMaybeTransactionType tsType, "description" .= i18n i tsType] <> resultDetails

          costs
            | selfOrigin = case subtotal of
                Nothing -> let total = - toInteger tsCost in ["cost" .= show (toInteger tsCost), "total" .= show total]
                Just st -> let total = st - toInteger tsCost
                          in ["subtotal" .= show st, "cost" .= show (toInteger tsCost), "total" .= show total]
            | otherwise = ["total" .= show (maybe 0 id subtotal)]

          encryptedCost = case tsSender of
            Just sender
              | sender == self -> case (tsType, tsResult) of
                  (Just TTEncryptedAmountTransfer, TxSuccess [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]) ->
                    ["encrypted" .= object ["encryptedAmount" .= neaEncryptedAmount,
                                            "newStartIndex" .= earUpToIndex,
                                            "newSelfEncryptedAmount" .= earNewAmount]]
                  (Just TTTransferToPublic, TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}]) ->
                    ["encrypted" .= object ["newStartIndex" .= earUpToIndex,
                                            "newSelfEncryptedAmount" .= earNewAmount]]
                  (Just TTTransferToEncrypted, TxSuccess [EncryptedSelfAmountAdded{..}]) ->
                    ["encrypted" .= object ["newSelfEncryptedAmount" .= eaaNewAmount]]
                  _ -> []
              | otherwise -> case (tsType, tsResult) of
                  (Just TTEncryptedAmountTransfer, TxSuccess [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]) ->
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
renderTransactionType TTTransfer = "transfer"
renderTransactionType TTAddBaker = "addBaker"
renderTransactionType TTRemoveBaker = "removeBaker"
renderTransactionType TTUpdateBakerAccount = "updateBakerAccount"
renderTransactionType TTUpdateBakerSignKey = "updateBakerSignKey"
renderTransactionType TTDelegateStake = "delegateStake"
renderTransactionType TTUndelegateStake = "undelegateStake"
renderTransactionType TTUpdateElectionDifficulty = "updateElectionDifficulty"
renderTransactionType TTUpdateBakerAggregationVerifyKey = "updateBakerAggregationVerifyKey"
renderTransactionType TTUpdateBakerElectionKey = "updateBakerElectionKey"
renderTransactionType TTUpdateAccountKeys = "updateAccountKeys"
renderTransactionType TTAddAccountKeys = "addAccountKeys"
renderTransactionType TTRemoveAccountKeys = "removeAccountKeys"
renderTransactionType TTEncryptedAmountTransfer = "encryptedAmountTransfer"
renderTransactionType TTTransferToEncrypted = "transferToEncrypted"
renderTransactionType TTTransferToPublic = "transferToPublic"

renderMaybeTransactionType :: Maybe TransactionType -> Text
renderMaybeTransactionType (Just tt) = renderTransactionType tt
renderMaybeTransactionType Nothing = "deployCredential"

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
    eventCost AmountAddedByDecryption{..} = Just $ toInteger aabdAmount
    eventCost EncryptedSelfAmountAdded{..} = Just $ - toInteger eaaAmount
    eventCost _ = Nothing

dropAmount :: Amount
dropAmount = 100000000


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

putGTUDropR :: Text -> Handler TypedContent
putGTUDropR addrText = do
    i <- internationalize
    case addressFromText addrText of
      Left _ -> respond400Error EMMalformedAddress RequestInvalid
      Right addr -> runGRPC (doGetAccInfo addrText) $ \case
          -- Account is not finalized
          Nothing -> sendResponseStatus notFound404 $ object
                      ["errorMessage" .= i18n i EMAccountNotFinal,
                       "error" .= fromEnum RequestInvalid]
          -- Account is finalized, so try the drop
          Just _ -> tryDrop addr

  where
    accountToText = Text.decodeUtf8 . addressToBytes
    doGetAccInfo :: Text -> ClientMonad IO (Either String (Maybe (Nonce, Amount)))
    doGetAccInfo t = do
      lastFinBlock <- getLastFinalBlockHash
      ai <- getAccountInfo t lastFinBlock
      return $ parseMaybe (withObject "account info" $ \o -> (,) <$> (o .: "accountNonce") <*> (o .: "accountAmount")) <$> ai
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
    tryDrop addr = do
      Proxy{..} <- getYesod
      let dropEnergy = simpleTransferEnergyCost (length dropKeys)
      let getNonce = (>>= parseEither (withObject "nonce" (.: "nonce"))) <$> getNextAccountNonce (accountToText dropAccount)
      -- Determine if there is already a GTU drop entry for this account
      rcpRecord <- runDB $ getBy (UniqueAccount (ByteStringSerialized addr))
      case rcpRecord of
        -- If there is no entry, try the drop
        Nothing -> runGRPC getNonce $ \nonce -> do
          currentTime <- liftIO $ round <$> getPOSIXTime
          let
            payload = Transfer addr dropAmount
            btrPayload = encodePayload payload
            btrHeader = TransactionHeader {
              thSender = dropAccount,
              thNonce = nonce,
              thEnergyAmount = dropEnergy,
              thPayloadSize = payloadSize btrPayload,
              thExpiry = TransactionExpiryTime $ currentTime + 300
            }
            transaction = signTransaction dropKeys btrHeader btrPayload
          mk <- runDB $ insertUnique (GTURecipient (ByteStringSerialized addr) (ByteStringSerialized transaction))
          case mk of
            -- There is already a transaction, so retry.
            Nothing -> tryDrop addr
            Just _ -> sendTransaction transaction
        Just (Entity key (GTURecipient _ (ByteStringSerialized transaction))) ->
          runGRPC (doGetAccInfo (accountToText dropAccount)) $ \case
              Nothing -> do
                $(logError) $ "Could not get GTU drop account info."
                configErr
              Just (lastFinNonce, lastFinAmt) -> do
                let trHash = getHash (NormalTransaction transaction) :: TransactionHash
                runGRPC (doIsTransactionOK trHash) $ \case
                  True -> sendResponse (object ["submissionId" .= trHash])
                  False
                    | lastFinAmt < dropAmount + fromIntegral dropEnergy
                      -> do
                        $(logError) $ "GTU drop account has insufficient funds"
                        configErr
                    | lastFinNonce > thNonce (btrHeader transaction)
                      -> do
                        -- Given the nonce, the transaction is no good. Delete and try again.
                        runDB $ delete key
                        tryDrop addr
                    | otherwise
                      -> do
                        currentTime <- liftIO $ round <$> getPOSIXTime
                        if thExpiry (btrHeader transaction) < TransactionExpiryTime currentTime then do
                          runDB $ delete key
                          tryDrop addr
                        else sendTransaction transaction

getGlobalFileR :: Handler TypedContent
getGlobalFileR = toTypedContent . globalInfo <$> getYesod

getIpsR :: Handler TypedContent
getIpsR = toTypedContent . ipInfo <$> getYesod
