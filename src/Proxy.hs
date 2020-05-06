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
import Network.HTTP.Types(badRequest400, forbidden403, badGateway502)
import Yesod hiding (InternalError)
import qualified Yesod
import Database.Persist.Sql
import Data.Maybe(catMaybes)
import Data.Time.Clock.POSIX

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Execution
import Concordium.Client.GRPC
import Concordium.ID.Types (addressFromText, addressToBytes, KeyIndex)
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.GlobalState.SQL.AccountTransactionIndex
import Concordium.GlobalState.SQL

import Internationalization

data ErrorCode = InternalError | RequestInvalid
    deriving(Eq, Show, Enum)

data Proxy = Proxy {
  grpcEnvData :: !EnvData,
  dbConnectionPool :: ConnectionPool,
  dropAccount :: AccountAddress,
  dropKeys :: [(KeyIndex, KeyPair)]
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
/accBalance/#Text AccountBalanceR GET
/accNonce/#Text AccountNonceR GET
/accTransactions/#Text AccountTransactionsR GET
/simpleTransferCost SimpleTransferCostR GET
/submissionStatus/#Text SubmissionStatusR GET
/submitCredential/ CredentialR PUT
/submitTransfer/ TransferR PUT
/testnetGTUDrop/#Text GTUDropR PUT
|]

respond400Error :: ErrorMessage -> ErrorCode -> Handler TypedContent
respond400Error err code = do
  i <- internationalize
  sendResponseStatus badRequest400 $
    object ["errorMessage" .= i18n i err,
            "error" .= fromEnum code
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
          getBal :: Value -> Maybe Integer
          getBal = parseMaybe (withObject "account info" (.: "accountAmount")) 
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

getSimpleTransferCostR :: Handler TypedContent
getSimpleTransferCostR = sendResponse $ object ["cost" .= Yesod.Number 59]

putCredentialR :: Handler TypedContent
putCredentialR = 
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success cdi -> do
          runGRPC (sendTransactionToBaker (CredentialDeployment cdi) defaultNetId) $ \case
            False -> do -- this case cannot happen at this time
              $(logError) "Credential rejected by node."
              respond400Error EMCredentialRejected RequestInvalid
            True -> 
              sendResponse (object ["submissionId" .= (getHash (CredentialDeployment cdi) :: TransactionHash)])

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
              return ["outcome" .= String "success", "to" .= addr, "amount" .= etAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of simple transfer: " ++ show es
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
                        Just (Text.unpack . Text.toLower -> ('d':_)) -> (Desc EntryId, "descending", (<.))
                        _ -> (Asc EntryId, "ascending", (>.))
      startId :: Maybe EntryId <- (>>= fromPathPiece) <$> lookupGetParam "from"
      let (startFilter, fromField) = maybe ([], []) (\sid -> ([ordRel EntryId sid], ["from" .= sid])) startId
      let addrFilter = [EntryAccount ==. ByteStringSerialized addr]
      limit <- maybe 20 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"
      entries <- runDB $ selectList (addrFilter <> startFilter) [ordering, LimitTo limit]
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
          fromField

formatEntry :: I18n -> AccountAddress -> Entity Entry -> Either String Value
formatEntry i self = \(Entity key Entry{..}) -> do
  let bHash = unBSS entryBlock
  let blockDetails = ["blockHash" .= bHash, "blockTime" .= timestampToSeconds entryBlockTime]
  transactionDetails <- case entryHash of
    Just tHashBS -> do
      let tHash = unBSS tHashBS
      TransactionSummary{..} :: TransactionSummary <- AE.eitherDecodeStrict entrySummary
      let (origin, selfOrigin) = case tsSender of
            Just sender
              | sender == self -> (object ["type" .= ("self" :: Text)], True)
              | otherwise -> (object ["type" .= ("account" :: Text), "address" .= sender], False)
            Nothing -> (object ["type" .= ("none" :: Text)], False)
      let (resultDetails, subtotal) = case tsResult of
            TxSuccess evts -> ((["outcome" .= ("success" :: Text), "events" .= fmap (i18n i) evts]
              <> case (tsType, evts) of
                  (Just TTTransfer, [Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr)]) ->
                    ["transferSource" .= fromAddr, "transferDestination" .= toAddr, "transferAmount" .= amt]
                  _ -> []), eventSubtotal self evts)
            TxReject reason -> (["outcome" .= ("reject" :: Text), "rejectReason" .= i18n i reason], Nothing)
      let details = object $ ["type" .= renderMaybeTransactionType tsType, "description" .= i18n i tsType] <> resultDetails
      let costs
            | selfOrigin = case subtotal of
                Nothing -> ["cost" .= toInteger tsCost, "total" .= (- toInteger tsCost)]
                Just st -> ["subtotal" .= st, "cost" .= toInteger tsCost, "total" .= (st - toInteger tsCost)]
            | otherwise = ["total" .= maybe 0 id subtotal]
      return $ [
        "origin" .= origin,
        "energy" .= tsEnergyCost,
        "details" .= details,
        "transactionHash" .= tHash
        ] <> costs
    Nothing -> do
      sto <- AE.eitherDecodeStrict entrySummary
      return $ case sto of
        BakingReward{..} -> [
          "origin" .= object ["type" .= ("reward" :: Text)],
          "total" .= stoRewardAmount,
          "details" .= object [
            "type" .= ("bakingReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription sto),
            "events" .= [i18n i sto]]]
  return $ object $ ["id" .= key] <> blockDetails <> transactionDetails

renderTransactionType :: TransactionType -> Text
renderTransactionType TTDeployModule = "deployModule"
renderTransactionType TTInitContract = "initContract"
renderTransactionType TTUpdate = "update"
renderTransactionType TTTransfer = "transfer"
renderTransactionType TTDeployEncryptionKey = "deployEncryptionKey"
renderTransactionType TTAddBaker = "addBaker"
renderTransactionType TTRemoveBaker = "removeBaker"
renderTransactionType TTUpdateBakerAccount = "updateBakerAccount"
renderTransactionType TTUpdateBakerSignKey = "updateBakerSignKey"
renderTransactionType TTDelegateStake = "delegateStake"
renderTransactionType TTUndelegateStake = "undelegateStake"
renderTransactionType TTUpdateElectionDifficulty = "updateElectionDifficulty"

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
    eventCost _ = Nothing



{-
1.  Lookup the account on the chain
        - If it's not finalized, AccountNotFinal
2.  Lookup the account in the database
    A. If there is an entry,
        A.1. Query the send account's last finalized nonce
        A.2. determine the state of the transaction
        - finalized, success: report complete
        - finalized, failed: remove db entry; retry 2.
        - committed/pending: report in progress (can return transaction hash for query)
        - absent:
            - if the transaction is expired or the nonce is below the next nonce, delete the DB entry; retry 2.
            - otherwise, resend the transaction and report in progress.
    B. If there is no entry
        B.1. query the sender's next available nonce
        B.2. produce/sign the transaction
        B.3. store the transaction in the database
            - on failure, retry from 2
        B.4. submit the transaction and report in progress.
-}

dropAmount :: Amount
dropAmount = 1000000

dropEnergy :: Energy
dropEnergy = 1000

putGTUDropR :: Text -> Handler TypedContent
putGTUDropR addrText = do
    i <- internationalize
    case addressFromText addrText of
      Left _ -> respond400Error EMMalformedAddress RequestInvalid
      Right addr -> runGRPC (doGetAccInfo addrText) $ \case
          -- Account is not finalized
          Nothing -> sendResponseStatus forbidden403 $ object
                      ["errorMessage" .= i18n i EMAccountNotFinal,
                       "error" .= fromEnum RequestInvalid]
          -- Account is finalized, so try the drop
          Just _ -> tryDrop addr

  where
    accountToText = Text.decodeUtf8 . addressToBytes
    doGetAccInfo :: Text -> ClientMonad IO (Either String (Maybe (Nonce, Amount)))
    doGetAccInfo t = do
      status <- either fail return =<< getConsensusStatus
      lastFinBlock <- liftResult $ parse readLastFinalBlock status
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
            $(logError) "Credential rejected by node."
            respond400Error EMCredentialRejected RequestInvalid
          True -> 
            sendResponse (object ["submissionId" .= (getHash (NormalTransaction transaction) :: TransactionHash)])
    liftResult (Success s) = return s
    liftResult (Error err) = fail err
    configErr = do
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMConfigurationError,
        "error" .= fromEnum InternalError
        ]
    tryDrop addr = do
      Proxy{..} <- getYesod
      let getNonce = (>>= parseEither (withObject "nonce" (.: "nonce"))) <$> getNextAccountNonce (accountToText dropAccount)
      -- Determine if there is already a GTU drop entry for this account
      rcpRecord <- runDB $ getBy (UniqueAccount (ByteStringSerialized addr))
      case rcpRecord of
        -- If there is no entry, try the drop
        Nothing -> runGRPC getNonce $ \nonce -> do
          currentTime <- liftIO $ round <$> getPOSIXTime
          let
            payload = Transfer (AddressAccount addr) dropAmount
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


