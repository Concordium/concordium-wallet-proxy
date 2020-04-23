{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Proxy where

import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString as BS

import Text.Read
import Control.Arrow (left)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Fail(MonadFail)
import Control.Exception (SomeException, catch)
import Data.Aeson(withObject, object, (.=), fromJSON, Result(..))
import Data.Aeson.Types(parse, parseMaybe)
import Data.Aeson.Parser(json')
import qualified Data.Aeson as AE
import Data.Conduit(connect)
import qualified Data.Serialize as S
import Data.Conduit.Attoparsec  (sinkParserEither)
import Network.HTTP.Types(badRequest400, badGateway502)
import Yesod hiding (InternalError)
import qualified Yesod
import Database.Persist.Sql
import Data.Maybe(catMaybes)

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Client.TransactionStatus
import Concordium.Types.Execution
import Concordium.Client.GRPC
import Concordium.ID.Types (addressFromText)
import Concordium.GlobalState.SQLiteATI


data ErrorCode = InternalError | RequestInvalid
    deriving(Eq, Show, Enum)

data Proxy = Proxy !EnvData ConnectionPool
instance Yesod Proxy where
  errorHandler e = return $ toTypedContent $ object [
        "errorMessage" .= msg,
        "error" .= fromEnum code
      ]
    where
      (msg, code) = case e of
        NotFound -> ("Not found", RequestInvalid)
        Yesod.InternalError{} -> ("Internal server error", InternalError)
        InvalidArgs{} -> ("Invalid arguments", RequestInvalid)
        NotAuthenticated -> ("Not logged in", RequestInvalid)
        (PermissionDenied reason) -> ("Permission denied: " <> reason, RequestInvalid)
        (BadMethod _) -> ("Bad method", RequestInvalid)

instance YesodPersist Proxy where
    type YesodPersistBackend Proxy = SqlBackend

    runDB action = do
        Proxy _ pool <- getYesod
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
|]

respond400Error :: ToJSON a => a -> ErrorCode -> Handler TypedContent
respond400Error err code =
  sendResponseStatus badRequest400 $
    object ["errorMessage" .= err,
            "error" .= fromEnum code
           ]

runGRPC :: ClientMonad IO (Either String a) -> (a -> Handler TypedContent) -> Handler TypedContent
runGRPC c k = do
  Proxy cfg _ <- getYesod
  let
    exHandler :: SomeException -> IO (Either String a)
    exHandler = pure . Left . show
  liftIO ((left show <$> runClient cfg c) `catch` exHandler) >>= \case
    Left err -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack err
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= Yesod.String "Error accessing the GRPC endpoint",
        "error" .= fromEnum InternalError
        ]
    Right (Left err) -> respond400Error err RequestInvalid
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
    Left err -> respond400Error (show err) RequestInvalid
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error err RequestInvalid
        Success cdi -> do
          runGRPC (sendTransactionToBaker (CredentialDeployment cdi) defaultNetId) $ \case
            False -> do -- this case cannot happen at this time
              $(logError) "Credential rejected by node."
              respond400Error ("Credential rejected by node." :: Text.Text) RequestInvalid
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
    Left err -> respond400Error (show err) RequestInvalid
    Right txJSON ->
      case parse transferParser txJSON  of
        Error err -> respond400Error err RequestInvalid
        Success tx -> do
          $(logInfo) (Text.pack (show tx))
          runGRPC (sendTransactionToBaker (NormalTransaction tx) defaultNetId) $ \case
            False -> do -- this case cannot happen at this time
              $(logError) "Credential rejected by node."
              respond400Error ("Credential rejected by node." :: Text.Text) RequestInvalid
            True -> 
              sendResponse (object ["submissionId" .= (getHash (NormalTransaction tx) :: TransactionHash)])
      where transferParser = withObject "Parse transfer request." $ \obj -> do
              sig :: TransactionSignature <- obj .: "signatures"
              body <- decodeBase16 =<< (obj .: "transaction")
              case S.decode ((S.encode sig) <> body) of
                Left err -> fail err
                Right tx -> return tx

-- Get the status of the submission.
getSubmissionStatusR :: Text -> Handler TypedContent
getSubmissionStatusR submissionId =
  case readMaybe (Text.unpack submissionId) of
    Nothing -> respond400Error ("Malformed transaction hash." :: Text.Text) RequestInvalid
    Just txHash -> runGRPC (getSimpleTransactionStatus txHash) (sendResponse . toJSON)

getAccountTransactionsR :: Text -> Handler TypedContent
getAccountTransactionsR addrText =
  case addressFromText addrText of
    Left err -> respond400Error ("Malformed account address: " <> Text.pack err) RequestInvalid
    Right addr -> do
      let addrFilter = [EntryAccount ==. S.encode addr]
      limit <- maybe 100 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"
      offset <- maybe 0 (max 0) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "offset"
      order <- lookupGetParam "order"
      let (ordering, ordType :: Text) = case order of
                        Just (Text.unpack . Text.toLower -> ('d':_)) -> (Desc EntryId, "descending")
                        _ -> (Asc EntryId, "ascending")
      entries <- runDB $ selectList addrFilter [ordering, LimitTo limit, OffsetBy offset]
      case mapM (formatEntry addr) entries of
        Left err -> do
          $(logError) $ "Error decoding transaction: " <> Text.pack err
          sendResponseStatus badGateway502 $ object [
            "errorMessage" .= Yesod.String "Database error",
            "error" .= fromEnum InternalError
            ]
        Right fentries -> sendResponse $ object ["offset" .= offset, "limit" .= limit, "order" .= ordType, "count" .= length fentries, "transactions" .= fentries]

formatEntry :: AccountAddress -> Entity Entry -> Either String Value
formatEntry self = \(entityVal -> Entry{..}) -> do
  bHash :: BlockHash <- S.decode entryBlock
  let blockDetails = ["block" .= bHash, "blockTime" .= entryBlockTime]
  transactionDetails <- case entryHash of
    Just tHashBS -> do
      tHash :: TransactionHash <- S.decode tHashBS
      TransactionSummary{..} :: TransactionSummary <- AE.eitherDecodeStrict entrySummary
      let (origin, selfOrigin) = case tsSender of
            Just sender
              | sender == self -> (object ["type" .= ("self" :: Text)], True)
              | otherwise -> (object ["type" .= ("account" :: Text), "address" .= sender], False)
            Nothing -> (object ["type" .= ("none" :: Text)], False)
      let (resultDetails, subtotal) = case tsResult of
            TxSuccess evts -> ((["outcome" .= ("success" :: Text), "events" .= fmap descrEvent evts]
              <> case (tsType, evts) of
                  (Just TTTransfer, [Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr)]) ->
                    ["transferSource" .= fromAddr, "transferDestination" .= toAddr, "transferAmount" .= amt]
                  _ -> []), eventSubtotal self evts)
            TxReject reason -> (["outcome" .= ("reject" :: Text), "rejectReason" .= descrRejectReason reason], Nothing)
      let details = object $ ["type" .= renderMaybeTransactionType tsType, "description" .= descrMaybeTransactionType tsType] <> resultDetails
      let costs
            | selfOrigin = case subtotal of
                Nothing -> ["fee" .= (- toInteger tsCost), "total" .= (- toInteger tsCost)]
                Just st -> ["subtotal" .= st, "fee" .= (- toInteger tsCost), "total" .= (st - toInteger tsCost)]
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
            "description" .= descrBakingReward stoBakerId,
            "events" .= [descrSpecialEvent sto]]]
  return $ object $ blockDetails <> transactionDetails

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

descrBakingReward :: BakerId -> Text
descrBakingReward bid = "Baking reward for baker " <> descrBaker bid

descrModule :: Core.ModuleRef -> Text
descrModule = Text.pack . show

descrContractRef :: Core.ModuleRef -> Core.TyName -> Text
descrContractRef mref (Core.TyName tyname) = descrModule mref <> ":" <> Text.pack (show tyname)

descrAccount :: AccountAddress -> Text
descrAccount = Text.pack . show

descrInstance :: ContractAddress -> Text
descrInstance = Text.pack . show

descrAmount :: Amount -> Text
descrAmount = Text.pack . show

descrBaker :: BakerId -> Text
descrBaker = Text.pack . show

descrAddress :: Address -> Text
descrAddress (AddressAccount addr) = descrAccount addr
descrAddress (AddressContract caddr) = descrInstance caddr

descrRejectReason :: RejectReason -> Text
descrRejectReason ModuleNotWF = "Typechecking of module failed" -- ^Error raised when typechecking of the module has failed.
descrRejectReason MissingImports = "Module has missing imports"
descrRejectReason (ModuleHashAlreadyExists mref) = "A module with the hash " <> descrModule mref <> " already exists"
descrRejectReason MessageTypeError = "Typechecking of smart contract message failed"
descrRejectReason ParamsTypeError = "Typechecking of smart contract initial parameters failed"
descrRejectReason (InvalidAccountReference addr) = "The account " <> descrAccount addr <> " does not exist"
descrRejectReason (InvalidContractReference mref tyname) = "Invalid smart contract reference: " <> descrContractRef mref tyname
descrRejectReason (InvalidModuleReference mref) = "Module does not exist: " <> descrModule mref
descrRejectReason (InvalidContractAddress caddr) = "No smart contract instance exists with address " <> descrInstance caddr
descrRejectReason (ReceiverAccountNoCredential addr) = "The receiving account (" <> descrAccount addr <> ") has has no valid credential"
descrRejectReason (ReceiverContractNoCredential caddr) = "The receiving smart contract instance (" <> descrInstance caddr <> "') has no valid credential"
descrRejectReason (AmountTooLarge addr _) = "The sending account (" <> descrAddress addr <> ") has insufficient funds"
descrRejectReason SerializationFailure = "Malformed transaction body" -- ^Serialization of the body failed.
descrRejectReason OutOfEnergy = "Insufficient energy"
descrRejectReason Rejected = "Rejected by contract logic"
descrRejectReason (AccountEncryptionKeyAlreadyExists _ _) = "The account encryption key already exists"
descrRejectReason (NonExistentRewardAccount addr) = "The designated reward account (" <> descrAccount addr <> ") does not exist"
descrRejectReason InvalidProof = "Invalid proof"
descrRejectReason (RemovingNonExistentBaker bid) = "Baker does not exist: " <> descrBaker bid
descrRejectReason (InvalidBakerRemoveSource _) = "Sender is not authorized to remove baker"
descrRejectReason (UpdatingNonExistentBaker bid) = "Baker does not exist: " <> descrBaker bid
descrRejectReason (InvalidStakeDelegationTarget bid) = "Baker does not exist: " <> descrBaker bid
descrRejectReason (DuplicateSignKey _) = "Duplicate baker signature key"
descrRejectReason (NotFromBakerAccount _ _) = "Sender is not the baker's designated account"
descrRejectReason NotFromSpecialAccount = "Sender is not authorized to perform chain control actions"

descrTransactionType :: TransactionType -> Text
descrTransactionType TTDeployModule = "Deploy a module"
descrTransactionType TTInitContract = "Initialize a smart contract"
descrTransactionType TTUpdate = "Invoke a smart contract"
descrTransactionType TTTransfer = "Transfer"
descrTransactionType TTDeployEncryptionKey = "Deploy account encryption key"
descrTransactionType TTAddBaker = "Add a baker"
descrTransactionType TTRemoveBaker = "Remove a baker"
descrTransactionType TTUpdateBakerAccount = "Update a baker's designated account"
descrTransactionType TTUpdateBakerSignKey = "Update a baker's signature key"
descrTransactionType TTDelegateStake = "Delegate stake to a baker"
descrTransactionType TTUndelegateStake = "Undelegate stake"
descrTransactionType TTUpdateElectionDifficulty = "Update leadership election difficulty parameter"

descrMaybeTransactionType :: Maybe TransactionType -> Text
descrMaybeTransactionType (Just tt) = descrTransactionType tt
descrMaybeTransactionType Nothing = "Deploy an account credential"

descrEvent :: Event -> Text
descrEvent (ModuleDeployed mref) = "Deployed module " <> descrModule mref
descrEvent (ContractInitialized mref tyname caddr amt) = "Initialized smart contract " <> descrContractRef mref tyname <> " at address " <> descrInstance caddr <> " with balance " <> descrAmount amt
descrEvent (Updated _ _ _ _) = "Invoked smart contract"
descrEvent (Transferred sender amt recv) = "Transferred " <> descrAmount amt <> " from " <> descrAddress sender <> " to " <> descrAddress recv
descrEvent (AccountCreated addr) = "Created account with address " <> descrAccount addr
descrEvent (CredentialDeployed _ addr) = "Deployed a credential to account " <> descrAccount addr
descrEvent (AccountEncryptionKeyDeployed _ addr) = "Deployed an encryption key to account " <> descrAccount addr
descrEvent (BakerAdded bid) = "Added baker " <> descrBaker bid
descrEvent (BakerRemoved bid) = "Removed baker " <> descrBaker bid
descrEvent (BakerAccountUpdated bid addr) = "Updated account for baker " <> descrBaker bid <> " to " <> descrAccount addr
descrEvent (BakerKeyUpdated bid _) = "Updated key for baker " <> descrBaker bid
descrEvent (BakerElectionKeyUpdated bid _) = "Updated election key for baker " <> descrBaker bid
descrEvent (StakeDelegated _ bid) = "Delegated stake to baker " <> descrBaker bid
descrEvent (StakeUndelegated _ _) = "Undelegated stake"
descrEvent (ElectionDifficultyUpdated diff) = "Updated leadership election difficulty to " <> Text.pack (show diff)

descrSpecialEvent :: SpecialTransactionOutcome -> Text
descrSpecialEvent BakingReward{..} = "Award " <> descrAmount stoRewardAmount <> " to baker " <> descrBaker stoBakerId <> " at " <> descrAccount stoBakerAccount

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