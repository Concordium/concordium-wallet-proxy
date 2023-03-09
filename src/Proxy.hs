{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
module Proxy where

import Database.Persist.Postgresql
import Database.Persist.Postgresql.JSON()
import Database.Persist.TH

import Data.Ratio
import Data.Bits
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as CH
import Control.Applicative

import qualified Data.Proxy as Proxy
import Data.Version (showVersion)
import qualified Data.Ratio as Rational
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Text.Read hiding (String)
import Control.Arrow (first, left)
import Control.Monad.Except
import Data.Functor
import Control.Exception (SomeException, catch)
import Data.Aeson(withObject, fromJSON, Result(..))
import Data.Aeson.Types(parse, Pair, parseEither)
import Data.Aeson.Parser(json')
import qualified Data.Aeson as AE
import Data.Conduit(connect)
import qualified Data.Serialize as S
import Data.Conduit.Attoparsec  (sinkParserEither)
import Network.HTTP.Types(badRequest400, notFound404, badGateway502)
import Network.GRPC.HTTP2.Types (GRPCStatusCode(..))
import Yesod hiding (InternalError)
import qualified Yesod
import Web.Cookie
import Data.Maybe(catMaybes, fromMaybe, isJust, maybeToList)
import Data.Either (fromRight)
import Data.Time.Clock.POSIX
import Data.Time.Format
import qualified Database.Esqueleto.Legacy as E
import qualified Database.Esqueleto.PostgreSQL.JSON as EJ
import qualified Database.Esqueleto.Internal.Internal as EInternal
import System.Random
import Data.Foldable
import Data.Word
import Data.Range
import Lens.Micro.Platform hiding ((.=))

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock as Clock

import Paths_wallet_proxy (version)
import Concordium.Types
import Concordium.Types.Queries hiding (Summary)
import Concordium.Types.Accounts
import Concordium.Types.Block
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Types.Transactions
import Concordium.Utils.Serialization (getMaybe)
import Concordium.Types.Execution
import qualified Concordium.Types.InvokeContract as InvokeContract
import qualified Concordium.Wasm                 as Wasm

import Concordium.Client.GRPC
import Concordium.Client.GRPC2
import Concordium.Client.Runner.Helper
import Concordium.Client.Types.Transaction(transferWithScheduleEnergyCost,
                                           transferWithSchedulePayloadSize,
                                           simpleTransferEnergyCost,
                                           simpleTransferPayloadSize,
                                           encryptedTransferEnergyCost,
                                           encryptedTransferPayloadSize,
                                           accountEncryptEnergyCost,
                                           accountEncryptPayloadSize,
                                           accountDecryptEnergyCost,
                                           minimumCost,
                                           accountDecryptPayloadSize, delegationConfigureEnergyCost, registerDelegationPayloadSize, updateDelegationPayloadSize, removeDelegationPayloadSize, bakerConfigurePayloadSize, bakerConfigureEnergyCostWithKeys, bakerConfigureEnergyCostWithoutKeys
                                           )
import Concordium.ID.Types (addressFromText, addressToBytes, KeyIndex, CredentialIndex)
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.Crypto.SHA256 (Hash)
import Concordium.Crypto.ByteStringHelpers (ByteStringHex(..))
import Concordium.Common.Version
import qualified Logging

import Internationalization

-- |Wraps a type for persistent storage via a serialization to a 'ByteString'.
newtype ByteStringSerialized a = ByteStringSerialized { unBSS :: a }
    deriving newtype (S.Serialize, Eq, Ord, Show)

instance S.Serialize a => PersistField (ByteStringSerialized a) where
  toPersistValue = toPersistValue . S.encode
  fromPersistValue =
    fromPersistValue >=> left (Text.pack) . S.decode

instance S.Serialize a => PersistFieldSql (ByteStringSerialized a) where
  sqlType _ = sqlType (Proxy.Proxy :: Proxy.Proxy BS.ByteString)

newtype TokenId = TokenId {unTokenId :: BSS.ShortByteString}
    deriving(Eq, Show)
    deriving (AE.ToJSON, AE.FromJSON) via ByteStringHex

instance PersistField TokenId where
  toPersistValue = toPersistValue . BSS.fromShort . unTokenId
  fromPersistValue = fmap (TokenId . BSS.toShort) . fromPersistValue

instance PersistFieldSql TokenId where
  sqlType _ = sqlType (Proxy.Proxy :: Proxy.Proxy BS.ByteString)

instance S.Serialize TokenId where
  put (TokenId tid) = S.putWord8 (fromIntegral (BSS.length tid)) <> S.putShortByteString tid
  get = do
    len <- S.getWord8
    TokenId <$> S.getShortByteString (fromIntegral len)

-- |Create the database schema and types. This creates a type called @Summary@
-- with fields @summaryBlock@, @summaryTimestamp@, etc., with stated types.
-- Analogously for @Entry@ and @ContractEntry@.
-- It also generates functionality for retrieving these records from SQL rows.
-- This is used below when querying the database (e.g., Entity Summary).
share [mkPersist sqlSettings] [persistLowerCase|
  Summary sql=summaries
    block (ByteStringSerialized BlockHash)
    timestamp Timestamp
    height AbsoluteBlockHeight
    summary AE.Value
    deriving Eq Show

  Entry sql=ati
    account (ByteStringSerialized AccountAddress)
    summary SummaryId
    deriving Eq Show

  ContractEntry sql=cti
    index ContractIndex
    subindex ContractSubindex
    summary SummaryId
    deriving Eq Show

  CIS2Entry sql=cis2_tokens
    index ContractIndex
    subindex ContractSubindex
    token_id TokenId
    total_supply (Ratio Integer)
  |]


data ErrorCode = InternalError | RequestInvalid | DataNotFound | ConversionError
    deriving(Eq, Show, Enum)

-- |Configuration for the @appSettings@ endpoint that returns whether the app is
-- out of date or not.
data ForcedUpdateConfig = ForcedUpdateConfig {
  -- |Versions which are forced to update.
  fucForceUpdate :: ![Range Word],
  -- |Versions which are going to be suggested to update.
  fucSuggestUpdate :: ![Range Word],
  -- |URL to update to if the version matches any of the above.
  fucURL :: !String,
  -- |URL to update to if the version matches the suggest update but not the
  -- forced update.
  fucSuggestURL :: !String
  } deriving (Show)

data Proxy = Proxy {
  grpcEnvData :: !EnvData,
  dbConnectionPool :: ConnectionPool,
  gtuDropData :: Maybe GTUDropData,
  forcedUpdateConfigIOSV0 :: Maybe ForcedUpdateConfig,
  forcedUpdateConfigAndroidV0 :: Maybe ForcedUpdateConfig,
  forcedUpdateConfigIOSV1 :: Maybe ForcedUpdateConfig,
  forcedUpdateConfigAndroidV1 :: Maybe ForcedUpdateConfig,
  healthTolerance :: Int,
  globalInfo :: Value,
  ipInfo :: Value,
  ipInfoV1 :: Value,
  logLevel :: Logging.LogLevel,
  -- |The version of terms and conditions currently in effect.
  -- If not set the endpoint termsAndConditionsVersion is disabled.
  tcVersion :: Maybe String,
  -- |URL to access terms and conditions.
  -- If not set the endpoint termsAndConditionsVersion is disabled.
  tcUrl :: Maybe String
}

-- | Data needed for GTU drops.
data GTUDropData = GTUDropData {
  -- | Account to send GTU from.
  dropAccount :: AccountAddress,
  -- | Keys for the account.
  dropKeys :: [(CredentialIndex, [(KeyIndex, KeyPair)])]
  }

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
/v0/accTransactions/#Text AccountTransactionsV0R GET
/v0/transactionCost TransactionCostR GET
/v0/submissionStatus/#Text SubmissionStatusR GET
/v0/submitCredential/ CredentialR PUT
/v0/submitTransfer/ TransferR PUT
/v0/testnetGTUDrop/#Text GTUDropR PUT
/v0/global GlobalFileR GET
/v0/health HealthR GET
/v0/ip_info IpsR GET
/v1/ip_info IpsV1R GET
/v1/accTransactions/#Text AccountTransactionsV1R GET
/v0/bakerPool/#Word64 BakerPoolR GET
/v0/chainParameters ChainParametersR GET
/v0/nextPayday NextPaydayR GET
/v0/passiveDelegation PassiveDelegationR GET
/v0/appSettings AppSettingsV0 GET
/v1/appSettings AppSettingsV1 GET
/v0/epochLength EpochLengthR GET
/v0/CIS2Tokens/#Word64/#Word64 CIS2Tokens GET
/v0/CIS2TokenMetadata/#Word64/#Word64 CIS2TokenMetadata GET
/v0/CIS2TokenBalance/#Word64/#Word64/#Text CIS2TokenBalance GET
/v0/termsAndConditionsVersion TermsAndConditionsVersion GET
|]

instance Yesod Proxy where
  -- Disable session handling entirely. We do not use sessions for anything at the moment.
  makeSessionBackend _ = return Nothing
  errorHandler e = do
    case e of
      Yesod.InternalError emsg -> $(logError) emsg
      _ -> return ()
    i <- internationalize
    return $
      toTypedContent $
        object
          [ "errorMessage" .= i18n i (EMErrorResponse e),
            "error" .= fromEnum code
          ]
    where
      code = case e of
        NotFound -> RequestInvalid
        Yesod.InternalError {} -> InternalError
        InvalidArgs {} -> RequestInvalid
        NotAuthenticated -> RequestInvalid
        PermissionDenied {} -> RequestInvalid
        BadMethod {} -> RequestInvalid

  shouldLogIO Proxy{..} _source level = return $ Logging.convertLogLevel level <= logLevel

-- |Terminate execution and respond with 400 status code with the given error
-- description.
respond400Error :: ErrorMessage -> ErrorCode -> Handler a
respond400Error err code = do
  i <- internationalize
  sendResponseStatus badRequest400 $
    object ["errorMessage" .= i18n i err,
            "error" .= fromEnum code
           ]

-- |Terminate execution and respond with 404 status code with the given error
-- description.
respond404Error :: ErrorMessage -> Handler a
respond404Error err = do
  i <- internationalize
  sendResponseStatus notFound404 $
    object ["errorMessage" .= i18n i err,
            "error" .= fromEnum DataNotFound
           ]

-- |Parse a set-cookie header string.
parseSetCookie' :: BS.ByteString -> SetCookie
parseSetCookie' c =
      let sc = parseSetCookie c
      in sc { setCookieExpires = setCookieExpires sc <|> (lookup "expires" flags >>= parseSetCookieExpires') }
  where
    breakDiscard :: Word8 -> BS.ByteString -> (BS.ByteString, BS.ByteString)
    breakDiscard w s =
      let (x, y) = BS.break (== w) s
      in (x, BS.drop 1 y)
    pairs = map (parsePair . dropSpace) $ BS.split 59 c ++ [BS8.empty] -- 59 = semicolon
    flags = map (first (BS8.map CH.toLower)) $ tail pairs
    parsePair = breakDiscard 61 -- equals sign
    dropSpace = BS.dropWhile (== 32) -- space
-- ^ we need this for addHeader since expiry fails to parse in
-- parseSetCookie in Web.Cookie.

-- |Parse a setcookie expires field.
parseSetCookieExpires' :: BS.ByteString -> Maybe UTCTime
parseSetCookieExpires' s = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X GMT" $ BS8.unpack s

-- |Run a GRPC request.
runGRPC :: ClientMonad IO (GRPCResult a)
        -> (a -> Handler TypedContent)
        -> Handler TypedContent
runGRPC c k = do
  cfg <- grpcEnvData <$> getYesod
  cookies <- getCookies
  let
    exHandler :: SomeException -> IO (Either String a, Map.Map BS.ByteString BS.ByteString)
    exHandler e = pure (Left $ show e, Map.empty)
  $(logOther "Trace") $ "Invoking queries with headers: " <> Text.pack (show cookies)
  liftIO (fmap (\(x, y) -> (left show x, y)) (runClientWithCookies cookies cfg c) `catch` exHandler) >>= \case
    (Left err, _) -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack err
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMGRPCError,
        "error" .= fromEnum InternalError
        ]
    (Right (Left err), _) -> do
      $(logError) $ "GRPC call failed: " <> Text.pack err
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i (EMGRPCErrorResponse err),
        "error" .= fromEnum RequestInvalid
        ]
    (Right (Right (GRPCResponse hds a)), updatedCookies) -> do
      $(logOther "Trace") $ "Got these response headers: " <> Text.pack (show hds)
      -- set cookies in response
      let scs = parseSetCookie' . snd <$> (filter $ ("set-cookie" ==) . fst) hds
      mapM_ setCookie scs
      $(logOther "Trace") $ "Set-cookies headers to be included in yesod response to client: " <> Text.pack (show scs)
      -- update cookie map
      cacheSet $! Map.union updatedCookies cookies
      k a
  where
    getYesodCookieMap :: Handler (Map.Map BS.ByteString BS.ByteString)
    getYesodCookieMap = Map.fromList
      . fmap (\(k',v') -> (Text.encodeUtf8 k', Text.encodeUtf8 v'))
      . reqCookies
      <$> getRequest
    getCookies :: Handler (Map.Map BS.ByteString BS.ByteString)
    getCookies = do
      -- map of cookies to be included in runClient
      sCookieMapM <- cacheGet
      -- yesod cookies in client request
      yCookieMap <- getYesodCookieMap
      case sCookieMapM of
        Nothing -> return yCookieMap
        Just scm -> return $ Map.union scm yCookieMap

runGRPCV2 :: ClientMonad IO (GRPCResultV2 (FromProtoResult a))
        -> (a -> Handler TypedContent)
        -> Handler TypedContent
runGRPCV2 = runGRPCV2WithNotFoundError Nothing

-- |Run a GRPC request.
runGRPCV2WithNotFoundError ::
           Maybe ErrorMessage -- ^ Error message to include in the response on GRPC status code NOT_FOUND.
        -> ClientMonad IO (GRPCResultV2 (FromProtoResult a)) 
        -> (a -> Handler TypedContent)
        -> Handler TypedContent
runGRPCV2WithNotFoundError errM c k = do
  cfg <- grpcEnvData <$> getYesod
  cookies <- getCookies
  $(logOther "Trace") $ "Invoking queries with headers: " <> Text.pack (show cookies)
  (res, updatedCookies) <- liftIO $ runClientWithCookies cookies cfg c
  case res of
    -- A client error occurred.
    Left clientError -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack (show clientError)
      i <- internationalize
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= i18n i EMGRPCError,
        "error" .= fromEnum InternalError
        ]
    -- Otherwise the HTTP/2 request succeeded and there is a GRPC result.
    Right r -> do
      case r of
        StatusOk (GRPCResponse hds convertedPayload) -> do
          case convertedPayload of
            Left err -> do
              $(logError) $ "Could not convert GRPC response payload: " <> Text.pack err
              i <- internationalize
              sendResponseStatus badGateway502 $ object [
                "errorMessage" .= i18n i (EMParseError err),
                "error" .= fromEnum ConversionError
                ]
            Right val -> do
              $(logOther "Trace") $ "Got these response headers: " <> Text.pack (show hds)
              -- set cookies in response
              let scs = parseSetCookie' . snd <$> (filter $ ("set-cookie" ==) . fst) hds
              mapM_ setCookie scs
              $(logOther "Trace") $ "Set-cookies headers to be included in yesod response to client: " <> Text.pack (show scs)
              -- update cookie map
              cacheSet $! Map.union updatedCookies cookies
              k val
        StatusInvalid -> do
          $(logError) "GRPC call failed: Invalid status code in response."
          i <- internationalize
          sendResponseStatus badGateway502 $ object [
            "errorMessage" .= i18n i (EMGRPCErrorResponse "Got invalid status code in response."),
            "error" .= fromEnum RequestInvalid
            ]
        StatusNotOk (NOT_FOUND, err) -> do
          $(logError) $ "GRPC call failed: Not found: " <> Text.pack err
          i <- internationalize
          sendResponseStatus notFound404 $ object [
            "errorMessage" .= i18n i (fromMaybe (EMGRPCErrorResponse $ "Got NOT_FOUND status code in GRPC response: " <> err) errM),
            "error" .= fromEnum RequestInvalid
            ]
        StatusNotOk (status, err) -> do
          $(logError) $ "GRPC call failed: Non-OK status code '" <> Text.pack (show status) <> "' in GRPC response: " <> Text.pack err
          i <- internationalize
          sendResponseStatus badGateway502 $ object [
            "errorMessage" .= i18n i (EMGRPCErrorResponse $ "Got non-OK status code '" <> show status <> "' in GRPC response."),
            "error" .= fromEnum RequestInvalid
            ]
        RequestFailed err -> do
          $(logError) $ "GRPC call failed: " <> Text.pack err
          i <- internationalize
          sendResponseStatus badGateway502 $ object [
            "errorMessage" .= i18n i (EMGRPCErrorResponse err),
            "error" .= fromEnum RequestInvalid
            ]
  where
    getYesodCookieMap :: Handler (Map.Map BS.ByteString BS.ByteString)
    getYesodCookieMap = Map.fromList
      . fmap (\(k',v') -> (Text.encodeUtf8 k', Text.encodeUtf8 v'))
      . reqCookies
      <$> getRequest
    getCookies :: Handler (Map.Map BS.ByteString BS.ByteString)
    getCookies = do
      -- map of cookies to be included in runClient
      sCookieMapM <- cacheGet
      -- yesod cookies in client request
      yCookieMap <- getYesodCookieMap
      case sCookieMapM of
        Nothing -> return yCookieMap
        Just scm -> return $ Map.union scm yCookieMap

firstPaydayAfter ::
  UTCTime -- ^Time of the next payday.
  -> Duration -- ^Duration of an epoch
  -> RewardPeriodLength -- ^Length of a payday.
  -> UTCTime -- ^Time at which the cooldown expires.
  -> UTCTime
firstPaydayAfter nextPayday epochDuration (RewardPeriodLength ep) cooldownEnd =
  if cooldownEnd <= nextPayday
  then nextPayday
  else let timeDiff = Clock.diffUTCTime cooldownEnd nextPayday
           paydayLength = durationToNominalDiffTime (fromIntegral ep * epochDuration)
           mult :: Word = ceiling (timeDiff / paydayLength)
       in Clock.addUTCTime (fromIntegral mult * paydayLength) nextPayday

pendingChangeToJSON :: AE.KeyValue kv => Maybe UTCTime -> Duration -> Maybe RewardPeriodLength -> StakePendingChange' UTCTime -> [kv]
pendingChangeToJSON _ _ _ NoChange = []
pendingChangeToJSON mnextPaydayTime epochDuration mrewardEpochs (ReduceStake amt eff) =
    [ "pendingChange"
        .= object
            (["change" .= String "ReduceStake",
              "newStake" .= amt,
              "effectiveTime" .= eff
             ] <> maybeToList ((\rewardEpochs nextPaydayTime -> "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs eff) <$> mrewardEpochs <*> mnextPaydayTime))
    ]
pendingChangeToJSON mnextPaydayTime epochDuration mrewardEpochs (RemoveStake eff) =
    [ "pendingChange"
        .= object
            (["change" .= String "RemoveStake",
              "effectiveTime" .= eff
             ] <> maybeToList ((\rewardEpochs nextPaydayTime -> "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs eff) <$> mrewardEpochs <*> mnextPaydayTime))
    ]

getRewardPeriodLength :: (MonadFail m, MonadIO m) => Text -> ClientMonad m (GRPCResult (Maybe RewardPeriodLength))
getRewardPeriodLength lfb = do
     let timeParametersParser = AE.withObject "Block summary" $ \obj -> do
           updates <- obj AE..: "updates"
           pv <- obj AE..: "protocolVersion"
           chainParameters <- updates AE..: "chainParameters"
           if pv >= P4 then
             Just <$> (chainParameters AE..: "rewardPeriodLength")
           else return Nothing
     summary <- either fail return =<< getBlockSummary lfb
     rpLength <- liftResult (parse timeParametersParser $ grpcResponseVal summary)
     return $ Right (GRPCResponse [] rpLength)

getRewardPeriodLengthV2 :: (MonadFail m, MonadIO m) => BlockHash -> ClientMonad m (GRPCResult (Maybe RewardPeriodLength))
getRewardPeriodLengthV2 lfb = do
  (EChainParametersAndKeys (ecpParams :: ChainParameters' cpv) _) <-
    either (fail . snd) return
      . getResponseValue
        =<< getBlockChainParametersV2 (Given lfb)
  let rpLength = case chainParametersVersion @cpv of
        SChainParametersV0 -> Nothing
        SChainParametersV1 -> Just $ ecpParams ^. cpTimeParameters . supportedOParam . tpRewardPeriodLength
        SChainParametersV2 -> Just $ ecpParams ^. cpTimeParameters . supportedOParam . tpRewardPeriodLength
  return $ Right (GRPCResponse [] rpLength)

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
getAccountBalanceR addrText = do
    runGRPCV2 doGetBal $ \(lastFinInfo, bestInfo, nextPayday, epochDuration, lastFinBlock) -> do
      let
          getBal :: AccountInfo -> Either (Maybe Value) (Maybe RewardPeriodLength -> Value)
          -- We're doing it in this low-level way to avoid parsing anything that
          -- is not needed, especially the encrypted amounts, since those are
          -- fairly expensive to parse.
          getBal AccountInfo{..} = do
            let balanceInfo = ["accountAmount" .= aiAccountAmount,
                                "accountEncryptedAmount" .= aiAccountEncryptedAmount,
                                "accountNonce" .= aiAccountNonce,
                                "accountReleaseSchedule" .= aiAccountReleaseSchedule,
                                "accountIndex" .= aiAccountIndex]
            case aiStakingInfo of
                  AccountStakingNone -> Left . Just $ object balanceInfo
                  AccountStakingBaker{..} -> do
                    let infoWithoutPending =
                          [
                            "stakedAmount" .= asiStakedAmount,
                            "restakeEarnings" .= asiStakeEarnings,
                            "bakerId" .= _bakerIdentity asiBakerInfo,
                            "bakerElectionVerifyKey" .= _bakerElectionVerifyKey asiBakerInfo,
                            "bakerSignatureVerifyKey" .= _bakerSignatureVerifyKey asiBakerInfo,
                            "bakerAggregationVerifyKey" .= _bakerAggregationVerifyKey asiBakerInfo
                          ]
                          <> maybe [] (\bpi -> ["bakerPoolInfo" .= bpi]) asiPoolInfo
                    case asiPendingChange of
                      NoChange -> Left . Just $ object $ balanceInfo <> ["accountBaker" .= object infoWithoutPending]
                      _ -> let bi rpl = object $ infoWithoutPending <> pendingChangeToJSON nextPayday epochDuration rpl asiPendingChange
                          in Right $ \rpl -> object (balanceInfo <> ["accountBaker" .= bi rpl])
                  AccountStakingDelegated{..} -> do
                    let infoWithoutPending = [
                            "stakedAmount" .= asiStakedAmount,
                            "restakeEarnings" .= asiStakeEarnings,
                            "delegationTarget" .= asiDelegationTarget
                          ]
                    case asiDelegationPendingChange of
                      NoChange -> Left . Just $ object $ balanceInfo <> ["accountDelegation" .= object infoWithoutPending]
                      _ -> let di rpl = object $ infoWithoutPending <> pendingChangeToJSON nextPayday epochDuration rpl asiDelegationPendingChange
                          in Right $ \rpl -> object (balanceInfo ++ ["accountDelegation" .= di rpl])
          lastFinBalComp = getBal lastFinInfo
          bestBalComp = getBal bestInfo
      let response lastFinBal bestBal = do
            $(logInfo) $ "Retrieved account balance for " <> addrText
                        <> ": finalizedBalance=" <> (Text.pack $ show lastFinBal)
                        <> ", currentBalance=" <> (Text.pack $ show bestBal)
            sendResponse $ object $ (maybe [] (\b -> ["finalizedBalance" .= b]) lastFinBal) <>
                              (maybe [] (\b -> ["currentBalance" .= b]) bestBal)
      case (lastFinBalComp, bestBalComp) of
        (Left lastFinBal, Left bestBal) -> response lastFinBal bestBal
        (Left lastFinBal, Right bestBalF) -> runGRPC (getRewardPeriodLengthV2 lastFinBlock) $ \rpl -> response lastFinBal (Just (bestBalF rpl))
        (Right lastFinBalF, Left bestBal) -> runGRPC (getRewardPeriodLengthV2 lastFinBlock) $ \rpl -> response (Just (lastFinBalF rpl)) bestBal
        (Right lastFinBalF, Right bestBalF) -> runGRPC (getRewardPeriodLengthV2 lastFinBlock) $ \rpl -> response (Just (lastFinBalF rpl)) (Just (bestBalF rpl))
  where
    -- VH/FIXME: Should this not ideally be a chain of handlers in some CPS style? In this
    --           case it seems better responses could be returned at each step like that,
    --           since "fail" seems to return status code 500. It seems like this may be the case
    --           in other places as well.
    doGetBal :: ClientMonad IO (GRPCResultV2 (FromProtoResult (AccountInfo, AccountInfo, Maybe UTCTime, Duration, BlockHash)))
    doGetBal = do
      accAddr <- parseAccountAddress addrText
      status <-
        either (fail . snd) return
          . getResponseValue
            =<< getConsensusInfoV2
      lastFinInfoRes <- getAccountInfoV2 (AccAddress accAddr) LastFinal
      (lastFinInfo, lfHds) <-
        either fail return
          $ getResponseValueAndHeaders lastFinInfoRes
      lastFinBlock <- getBlockHashHeader lfHds
      bestInfoRes <- getAccountInfoV2 (AccAddress accAddr) Best
      (bestInfo, biHds) <-
        either fail return
          $ getResponseValueAndHeaders bestInfoRes
      let (pv, epochDuration) = (csProtocolVersion status, csEpochDuration status)
      if pv >= P4 then do
        rewardStatusRes <- getTokenomicsInfoV2 (Given lastFinBlock)
        (rewardStatus, rsHds) <-
          either fail return
            $ getResponseValueAndHeaders rewardStatusRes 
        nextPayday <- case rewardStatus of
            RewardStatusV0{} -> fail "Invalid reward status."
            RewardStatusV1{..} -> return rsNextPaydayTime
        return $ StatusOk $ GRPCResponse rsHds $ Right (lastFinInfo, bestInfo, Just nextPayday, epochDuration, lastFinBlock)
      else do
        return $ StatusOk $ GRPCResponse biHds $ Right (lastFinInfo, bestInfo, Nothing, epochDuration, lastFinBlock)

-- |Helper function for parsing an account address.
-- VH/FIXME: Factor this out in Runner module and export
parseAccountAddress :: (MonadFail m) => Text -> m AccountAddress
parseAccountAddress accAddr =
  case addressFromText accAddr of
      Left _ -> fail "Unable to parse account address."
      Right a -> return a

doParseAccountAddress :: Text -> Handler AccountAddress
doParseAccountAddress addrText =
  case addressFromText addrText of
    Left err -> do
      let msg = "Invalid account address '" <> addrText <> "' for 'accountEncryptionKey' request: " <> Text.pack err
      $(logError) msg
      i <- internationalize
      sendResponseStatus badRequest400 $ object [
        "errorMessage" .= i18n i (EMParseError $ Text.unpack msg),
        "error"
         .= fromEnum ConversionError
        ]
    Right addr -> return addr

liftResult :: MonadFail m => Result a -> m a
liftResult (Success s) = return s
liftResult (Error err) = fail err

getAccountNonceR :: Text -> Handler TypedContent
getAccountNonceR addrText = do
  addr <- doParseAccountAddress addrText
  runGRPCV2WithNotFoundError (Just EMAccountDoesNotExist) (getNextSequenceNumberV2 addr) $ \nonce -> do
      $(logInfo) "Successfully got nonce."
      sendResponse $ toJSON nonce

-- |Get the account encryption key at the best block.
-- Return '404' status code if account does not exist in the best block at the moment.
getAccountEncryptionKeyR :: Text -> Handler TypedContent
getAccountEncryptionKeyR addrText = do
  addr <- doParseAccountAddress addrText
  runGRPCV2WithNotFoundError (Just EMAccountDoesNotExist) (getAccountInfoV2 (AccAddress addr) Best) $ \accInfo -> do
    let encryptionKey = aiAccountEncryptionKey accInfo
    $(logInfo) $ "Retrieved account encryption key for " <> addrText
            <> ": " <> Text.pack (show encryptionKey)
    sendResponse (object [ "accountEncryptionKey" .= encryptionKey ])

-- |Get the cost of a transaction, based on its type. The following query parameters are supported
-- - "type", the type of the transaction. This is mandatory.
-- - "numSignatures", the number of signatures on the transaction, defaults to 1 if not present.
-- - "memoSize", the size of the transfer memo. Only supported if the node is running protocol version 2 or higher, and
--   only applies when `type` is either `simpleTransfer` and `encryptedTransfer`.
getTransactionCostR :: Handler TypedContent
getTransactionCostR = withExchangeRate $ \(rate, pv) -> do
  numSignatures <- fromMaybe "1" <$> lookupGetParam "numSignatures"
  case readMaybe (Text.unpack numSignatures) of
    Just x | x > 0 -> handleTransactionCost pv rate x
    _ -> respond400Error (EMParseError "Could not parse `numSignatures` value.") RequestInvalid

  where
      handleTransactionCost pv rate numSignatures = do
        transactionType <- lookupGetParam "type"
        -- compute the additional size of the transaction based on the memo
        -- this only applies to transfer and encrypted transfer transaction types.
        memoPayloadSize <- do
          lookupGetParam "memoSize" >>= \case
            Nothing -> return 0
            Just memoText ->
              case readMaybe (Text.unpack memoText) :: Maybe Word32 of
                Nothing -> respond400Error (EMParseError "Could not parse `memoSize` value.") RequestInvalid
                -- NB: In protocol version 1 the memo is not supported. This
                -- implementation assumes that the transaction memo will be
                -- supported in all future versions of the node.
                -- The memo is charged for purely on its size. The "2 +" is there because the memo
                -- is serialized by prepending 2 bytes for its length.
                Just msize | pv /= P1 -> return $ fromIntegral (2 + msize)
                           | otherwise -> respond404Error EMActionNotCurrentlySupported
        let costResponse energyCost = sendResponse $ object ["cost" .= computeCost rate energyCost
                                      , "energy" .= energyCost
                                      ]
        case transactionType of
            Nothing -> respond400Error EMMissingParameter RequestInvalid
            Just tty -> case Text.unpack tty of
              "simpleTransfer" ->
                costResponse $ simpleTransferEnergyCost (simpleTransferPayloadSize + memoPayloadSize) numSignatures
              "encryptedTransfer" ->
                costResponse $ encryptedTransferEnergyCost (encryptedTransferPayloadSize + memoPayloadSize) numSignatures
              "transferToSecret" ->
                costResponse $ accountEncryptEnergyCost accountEncryptPayloadSize numSignatures
              "transferToPublic" ->
                costResponse $ accountDecryptEnergyCost accountDecryptPayloadSize numSignatures
              "registerDelegation" -> do
                isTargetPassiveDelegation <- isJust <$> lookupGetParam "passive"
                costResponse $
                  delegationConfigureEnergyCost
                    (registerDelegationPayloadSize isTargetPassiveDelegation)
                    numSignatures
              "updateDelegation" -> do
                isAmountUpdated <- isJust <$> lookupGetParam "amount"
                isRestakeUpdated <- isJust <$> lookupGetParam "restake"
                isTargetUpdated <- isJust <$> lookupGetParam "target"
                isTargetPassiveDelegation <- isJust <$> lookupGetParam "passive"
                let pSize = updateDelegationPayloadSize isAmountUpdated isRestakeUpdated isTargetUpdated isTargetPassiveDelegation
                costResponse $ delegationConfigureEnergyCost pSize numSignatures
              "removeDelegation" ->
                costResponse $ delegationConfigureEnergyCost removeDelegationPayloadSize numSignatures
              "registerBaker" -> do
                metasize <- lookupGetParam "metadataSize" >>= \case
                  Nothing -> return $ Just $ fromIntegral maxUrlTextLength
                  Just ms -> case readMaybe $ Text.unpack ms of
                    Nothing -> respond400Error (EMParseError "Could not parse `metadataSize` value.") RequestInvalid
                    Just v -> return $ Just v
                let pSize = bakerConfigurePayloadSize True True True True metasize True True True
                costResponse $ bakerConfigureEnergyCostWithKeys pSize numSignatures
              "updateBakerStake" -> do
                isAmountUpdated <- isJust <$> lookupGetParam "amount"
                isRestakeUpdated <- isJust <$> lookupGetParam "restake"
                let pSize = bakerConfigurePayloadSize isAmountUpdated isRestakeUpdated False False Nothing False False False
                costResponse $ bakerConfigureEnergyCostWithoutKeys pSize numSignatures
              "updateBakerPool" -> do
                metasize <- lookupGetParam "metadataSize" >>= \case
                  Nothing -> return Nothing
                  Just ms -> case readMaybe $ Text.unpack ms of
                    Nothing -> respond400Error (EMParseError "Could not parse `metadataSize` value.") RequestInvalid
                    Just v -> return $ Just v
                isOpenStatusUpdated <- isJust <$> lookupGetParam "openStatus"
                isTComUpdated <- isJust <$> lookupGetParam "transactionCommission"
                isBComUpdated <- isJust <$> lookupGetParam "bakerRewardCommission"
                isFComUpdated <- isJust <$> lookupGetParam "finalizationRewardCommission"
                let pSize = bakerConfigurePayloadSize False False isOpenStatusUpdated False metasize isTComUpdated isBComUpdated isFComUpdated
                costResponse $ bakerConfigureEnergyCostWithoutKeys pSize numSignatures
              "update" -> do
                invoker <- lookupGetParam "sender" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `sender` value.") RequestInvalid
                      Just val -> case addressFromText val of
                        Left s -> respond400Error (EMParseError $ "Could not parse `sender` value: " ++ s) RequestInvalid
                        Right addr -> return $ Just $ AddressAccount addr

                contractIndex <- lookupGetParam "contractIndex" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `contractIndex` value.") RequestInvalid
                      Just val -> case readMaybe $ Text.unpack val of
                        Nothing -> respond400Error (EMParseError "Could not parse `contractIndex` value.") RequestInvalid
                        Just index -> return $ ContractIndex index

                contractSubindex <- lookupGetParam "contractSubindex" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `contractSubindex` value.") RequestInvalid
                      Just val -> case readMaybe $ Text.unpack val of
                        Nothing -> respond400Error (EMParseError "Could not parse `contractSubindex` value.") RequestInvalid
                        Just index -> return $ ContractSubindex index

                let contract = ContractAddress{..}

                amount <- lookupGetParam "amount" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `amount` value.") RequestInvalid
                      Just val -> case readMaybe $ Text.unpack val of
                        Nothing -> respond400Error (EMParseError "Could not parse `amount` value.") RequestInvalid
                        Just a -> return a
                wasmReceiveName <- lookupGetParam "receiveName" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `receiveName` value.") RequestInvalid
                      Just receiveName -> if Wasm.isValidReceiveName receiveName
                        then return $ Wasm.ReceiveName receiveName
                        else respond400Error (EMParseError "Invalid receive name.") RequestInvalid
                wasmParameter <- lookupGetParam "parameter" >>= \case
                      Nothing -> respond400Error (EMParseError "Missing `parameter` value.") RequestInvalid
                      Just parameterText -> case BS16.decode . Text.encodeUtf8 $ parameterText of
                        Left s -> respond400Error (EMParseError $ "Could not parse `parameter` value: " ++ s) RequestInvalid
                        Right parameter -> return $ Wasm.Parameter $ BSS.toShort parameter

                (energyBufferPercentage :: Int) <- lookupGetParam "executionNRGBuffer" >>= \case
                      Nothing -> return 20
                      Just bufferText -> case readMaybe $ Text.unpack bufferText of
                        Nothing -> respond400Error (EMParseError "Could not parse `executionNRGBuffer` value.") RequestInvalid
                        Just n -> return n

                let nrg = Energy 500000 -- 500 thousands

                let pSize = 1 -- 1 byte for the payload tag
                          + 8 -- 8 bytes for the amount
                          + 16 -- 16 bytes for the contract address
                          + 2 -- 2 bytes for the length of receive name
                          + Text.length (Wasm.receiveName wasmReceiveName) -- the number of bytes inside the receive name
                          + 2 -- 2 bytes for the length of the parameter
                          + BSS.length (Wasm.parameter wasmParameter) -- the number of bytes inside the parameter
                let minCost = minimumCost (fromIntegral pSize) numSignatures
                let invokeContext = InvokeContract.ContractContext
                                    { ccInvoker = invoker
                                    , ccContract = contract
                                    , ccAmount = amount
                                    , ccMethod = wasmReceiveName
                                    , ccParameter = wasmParameter
                                    , ccEnergy = nrg
                                    }
                invokeContextArg <- case Text.decodeUtf8' . BSL.toStrict . AE.encode $ invokeContext of
                      Left _ -> respond400Error (EMParseError "Could not invoke contract due to internal error: decoding UTF-8 failed") RequestInvalid -- Should never happen.
                      Right text -> return text

                let getInvokeCost = withBestBlockHash Nothing $ \bb -> do
                      res <- invokeContract invokeContextArg bb
                      -- this is the C_t for the smart contract update transaction
                      return $ case res of
                            Left err -> Left $ "Invocation failed with error: " ++ show err
                            Right jsonValue -> case AE.fromJSON $ grpcResponseVal jsonValue of
                              AE.Error jsonErr -> Left $ "Invocation failed with error: " ++ show jsonErr
                              AE.Success InvokeContract.Failure{..} -> Right $ GRPCResponse (grpcHeaders jsonValue) rcrUsedEnergy
                              AE.Success InvokeContract.Success{..} -> Right $ GRPCResponse (grpcHeaders jsonValue) rcrUsedEnergy
                let cost invokeCost = minCost + (invokeCost + (invokeCost * fromIntegral energyBufferPercentage) `div` 100)
                runGRPC getInvokeCost $ costResponse . cost
              "updateBakerKeys" -> do
                let pSize = bakerConfigurePayloadSize False False False True Nothing False False False
                costResponse $ bakerConfigureEnergyCostWithKeys pSize numSignatures
              "removeBaker" -> do
                let pSize = bakerConfigurePayloadSize True False False False Nothing False False False
                costResponse $ bakerConfigureEnergyCostWithoutKeys pSize numSignatures
              "configureBaker" -> do
                metasize <- lookupGetParam "metadataSize" >>= \case
                  Nothing -> return Nothing
                  Just ms -> case readMaybe $ Text.unpack ms of
                    Nothing -> respond400Error (EMParseError "Could not parse `metadataSize` value.") RequestInvalid
                    Just v -> return $ Just v
                isAmountUpdated <- isJust <$> lookupGetParam "amount"
                isRestakeUpdated <- isJust <$> lookupGetParam "restake"
                isOpenStatusUpdated <- isJust <$> lookupGetParam "openStatus"
                areKeysUpdated <- isJust <$> lookupGetParam "keys"
                isTComUpdated <- isJust <$> lookupGetParam "transactionCommission"
                isBComUpdated <- isJust <$> lookupGetParam "bakerRewardCommission"
                isFComUpdated <- isJust <$> lookupGetParam "finalizationRewardCommission"
                let pSize = bakerConfigurePayloadSize isAmountUpdated isRestakeUpdated isOpenStatusUpdated areKeysUpdated metasize isTComUpdated isBComUpdated isFComUpdated
                costResponse $
                  (if areKeysUpdated then bakerConfigureEnergyCostWithKeys else bakerConfigureEnergyCostWithoutKeys)
                    pSize
                    numSignatures
              tty' -> respond400Error (EMParseError $ "Could not parse transaction type: " <> tty') RequestInvalid
      fetchUpdates :: ClientMonad IO (GRPCResultV2 (Either String (EnergyRate, ProtocolVersion)))
      fetchUpdates = do
        consensusInfoRes <- getConsensusInfoV2
        -- This extraction of the parameter is not ideal for two reasons
        -- - this is the exchange rate in the best block, which could already be obsolete.
        --   This is not likely not matter since block times 10s on average, and it is always the case
        --   that the transaction is committed after the current time. In any case this is only an estimate.
        -- - It is manually parsing the return value, instead of using the Updates type. This should be fixed
        --   and we want to use the same calculation in concordium-client, however that requires more restructuring
        --   of the dependencies. The current solution is good enough in the meantime.
        case getResponseValue consensusInfoRes of
            Left (_, err) -> fail err
            Right cs -> do
              chainParamsRes <- getBlockChainParametersV2 Best
              case getResponseValueAndHeaders chainParamsRes of
                Left err -> fail err
                Right (EChainParametersAndKeys (ecpParams :: ChainParameters' cpv) _, hds) -> do
                  return $ StatusOk $ GRPCResponse hds (Right (_erEnergyRate $ _cpExchangeRates ecpParams, csProtocolVersion cs))
      withExchangeRate = runGRPCV2 fetchUpdates

putCredentialR :: Handler TypedContent
putCredentialR =
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success Versioned{..} | vVersion == 0 -> do
          runGRPCV2 (doSendBlockItem (CredentialDeployment vValue)) $ \case
            False -> do -- this happens if the request is duplicate, stale, or malformed.
              $(logError) "Credential rejected by node."
              respond400Error EMCredentialRejected RequestInvalid
            True ->
              sendResponse (object ["submissionId" .= (getHash (CredentialDeployment vValue) :: TransactionHash)])
                              | otherwise -> respond400Error (EMParseError $ "Invalid version number " ++ show vVersion) RequestInvalid
  where doSendBlockItem item = do
          sbiRes <- sendBlockItemV2 item
          case getResponseValueAndHeaders sbiRes of
            Left _ -> return $ StatusOk $ GRPCResponse [] (Right False)
            Right (_, hds) -> return $ StatusOk $ GRPCResponse hds (Right True)

-- |Use the serialize instance of a type to deserialize
decodeBase16 :: (MonadFail m) => Text.Text -> m BS.ByteString
decodeBase16 t =
    case BS16.decode (Text.encodeUtf8 t) of
      Right bs -> return bs
      Left _ -> fail $ "Could not decode as base-16: " ++ show t

putTransferR :: Handler TypedContent
putTransferR =
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (EMParseError (show err)) RequestInvalid
    Right txJSON ->
      case parse transferParser txJSON  of
        Error err -> respond400Error (EMParseError err) RequestInvalid
        Success tx -> do
          $(logInfo) (Text.pack (show tx))
          runGRPCV2 (doSendBlockItem (NormalTransaction tx)) $ \case
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
            doSendBlockItem item = do
              sbiRes <- sendBlockItemV2 item
              case getResponseValueAndHeaders sbiRes of
                Left _ -> return $ StatusOk $ GRPCResponse [] (Right False)
                Right (_, hds) -> return $ StatusOk $ GRPCResponse hds (Right True)

getSimpleTransactionStatus :: MonadIO m => I18n -> TransactionHash -> ClientMonad m (GRPCResult Value)
getSimpleTransactionStatus i trHash = do
    eitherStatus <- getTransactionStatus (Text.pack $ show trHash)
    return $
      eitherStatus >>= \case
        GRPCResponse hds Null -> return $ GRPCResponse hds $ object ["status" .= String "absent"]
        GRPCResponse hds (Object o) -> do
          parseEither (.: "status") o >>= \case
            "received" -> return $ GRPCResponse hds $ object ["status" .= String "received"]
            "finalized" -> HM.toList <$> parseEither (.: "outcomes") o >>= \case
              [(bh,outcome)] -> do
                fields <- outcomeToPairs outcome
                return $ GRPCResponse hds $ object $ ["status" .= String "finalized", "blockHashes" .= [bh :: BlockHash]] <> fields
              _ -> throwError "expected exactly one outcome for a finalized transaction"
            "committed" -> do
              outcomes <- HM.toList <$> parseEither (.: "outcomes") o
              fields <- outcomesToPairs (snd <$> outcomes)
              return $ GRPCResponse hds $ object $ ["status" .= String "committed", "blockHashes" .= (fst <$> outcomes :: [BlockHash])] <> fields
            s -> throwError ("unexpected \"status\": " <> s)
        _ -> throwError "expected null or object"
  where
    -- attach a memo to the pairs.
    addMemo [TransferMemo memo] xs = ("memo" .= memo):xs
    addMemo _ xs = xs

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
        (viewTransfer -> True) -> -- transaction is either a transfer or transfer with memo
          case tsResult of
            TxSuccess (Transferred{etTo = AddressAccount addr,..}:mmemo) ->
              return $ addMemo mmemo
                ["outcome" .= String "success",
                 "to" .= addr,
                 "amount" .= etAmount]
            TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
            es ->
              Left $ "Unexpected outcome of simple transfer: " ++ show es
        (viewEncryptedTransfer -> True) ->
          case tsResult of
            TxSuccess (EncryptedAmountsRemoved{..}:NewEncryptedAmount{..}:mmemo) ->
              return $ addMemo mmemo ["outcome" .= String "success",
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
        TSTAccountTransaction (Just TTConfigureBaker) ->
            case tsResult of
                TxSuccess ((eventBakerId -> (Just bid)) : _) ->
                    return ["outcome" .= String "success",
                            "bakerId" .= bid]
                TxSuccess _ -> return ["outcome" .= String "success"]
                TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
        TSTAccountTransaction (Just TTConfigureDelegation) ->
            case tsResult of
                TxSuccess _ -> return ["outcome" .= String "success"]
                TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
        TSTAccountTransaction (Just TTDeployModule) ->
            case tsResult of
                TxSuccess [ModuleDeployed mref] ->
                    return ["outcome" .= String "success",
                            "moduleRef" .= mref]
                TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                es ->
                  Left $ "Unexpected outcome of deploying module: " ++ show es
        TSTAccountTransaction (Just TTInitContract) ->
            case tsResult of
                TxSuccess [ContractInitialized{..}] ->
                    return ["outcome" .= String "success",
                            "moduleRef" .= ecRef,
                            "address" .= ecAddress,
                            "amount" .= ecAmount,
                            "initName" .= ecInitName,
                            "contractVersion" .= ecContractVersion,
                            "events" .= ecEvents]
                TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                es ->
                  Left $ "Unexpected outcome of initialized module: " ++ show es
        TSTAccountTransaction (Just TTUpdate) ->
            case tsResult of
                TxSuccess events ->
                    case eventsToMaybeValues events of
                      Just vals -> return ["outcome" .= String "success",
                                           "trace" .= vals]
                      Nothing -> Left $ "Unexpected outcome of updating module: " ++ show tsResult
                TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
        _ -> case tsResult of
              TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
              _ -> Left "Unsupported transaction type for simple statuses."
    outcomesToPairs :: [TransactionSummary] -> Either String [Pair]
    outcomesToPairs l = do
      outcomes <- mapM outcomeToPairs l
      case outcomes of
        [] -> Left "Expected at least one transaction outcome for a committed transaction"
        [o] -> return o
        (h:r)
          | all (h==) r -> return h
          | otherwise -> return ["outcome" .= String "ambiguous"]
    -- This function returns the JSON representing an event that can occur due to a smart contract update transaction.
    -- It returns @Nothing@ if used on an event different from the four smart contract update events `Updated`, `Transferred`,
    -- `Interrupted`, `Resumed`.
    updateEventToMaybeValue :: Event -> Maybe Value
    updateEventToMaybeValue Updated{..} = Just $ object ["type" .= String "updated",
                                                 "address" .= euAddress,
                                                 "instigator" .= euInstigator,
                                                 "amount" .= euAmount,
                                                 "message" .= euMessage,
                                                 "receiveName" .= euReceiveName,
                                                 "contractVersion" .= euContractVersion,
                                                 "events" .= euEvents]
    updateEventToMaybeValue Transferred{etFrom = AddressContract addrFrom, etTo = AddressAccount addrTo, ..} = Just $ object ["type" .= String "transferred",
                                                            "from" .= addrFrom,
                                                            "amount" .= etAmount,
                                                            "to" .= addrTo]
    updateEventToMaybeValue Interrupted{..} = Just $ object ["type" .= String "interrupted",
                                                            "address" .= iAddress,
                                                            "events" .= iEvents]
    updateEventToMaybeValue Resumed{..} = Just $ object ["type" .= String "resumed",
                                                        "address" .= rAddress,
                                                        "success" .= rSuccess]
    updateEventToMaybeValue _ = Nothing
    -- This function maps the list of events to a (maybe) list of JSON values representing these events.
    -- It returns @Just@ if all the events are among the events `Updated`, `Transferred`,
    -- `Interrupted`, `Resumed`. Otherwise @Nothing@.
    -- It is only supposed to be called on a list of the above events.
    eventsToMaybeValues :: [Event] -> Maybe [Value]
    eventsToMaybeValues events = sequence $ updateEventToMaybeValue <$> events

-- helper functions to be used in view patterns to match both transfers and
-- transfers with memo.
viewTransfer :: TransactionSummaryType -> Bool
viewTransfer (TSTAccountTransaction (Just TTTransfer)) = True
viewTransfer (TSTAccountTransaction (Just TTTransferWithMemo)) = True
viewTransfer _ = False

viewEncryptedTransfer :: TransactionSummaryType -> Bool
viewEncryptedTransfer (TSTAccountTransaction (Just TTEncryptedAmountTransfer)) = True
viewEncryptedTransfer (TSTAccountTransaction (Just TTEncryptedAmountTransferWithMemo)) = True
viewEncryptedTransfer _ = False

viewScheduledTransfer :: TransactionSummaryType -> Bool
viewScheduledTransfer (TSTAccountTransaction (Just TTTransferWithSchedule)) = True
viewScheduledTransfer (TSTAccountTransaction (Just TTTransferWithScheduleAndMemo)) = True
viewScheduledTransfer _ = False

-- |Get the baker ID from a baker configuration event
eventBakerId :: Event -> Maybe BakerId
eventBakerId BakerAdded{..} = Just ebaBakerId
eventBakerId BakerRemoved{..} = Just ebrBakerId
eventBakerId BakerStakeIncreased{..} = Just ebsiBakerId
eventBakerId BakerStakeDecreased{..} = Just ebsiBakerId
eventBakerId BakerSetRestakeEarnings{..} = Just ebsreBakerId
eventBakerId BakerKeysUpdated{..} = Just ebkuBakerId
eventBakerId _ = Nothing

-- Get the status of the submission.
getSubmissionStatusR :: Text -> Handler TypedContent
getSubmissionStatusR submissionId =
  case readMaybe (Text.unpack submissionId) of
    Nothing -> respond400Error EMMalformedTransaction RequestInvalid
    Just txHash -> do
      i <- internationalize
      runGRPC (getSimpleTransactionStatus i txHash) (sendResponse . toJSON)

-- |Whether to include memos in formatted account transactions or not.
-- If not, transfers with memos are mapped to a corresponding transfer without a memo.
data IncludeMemos = IncludeMemo | ExcludeMemo
    deriving(Eq, Show)

getAccountTransactionsV0R :: Text -> Handler TypedContent
getAccountTransactionsV0R = getAccountTransactionsWorker ExcludeMemo

getAccountTransactionsV1R :: Text -> Handler TypedContent
getAccountTransactionsV1R = getAccountTransactionsWorker IncludeMemo

getCIS2Tokens :: Word64 -> Word64 -> Handler TypedContent
getCIS2Tokens index subindex = do
  limit <- maybe 20 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"
  mfrom :: Maybe CIS2EntryId <- (>>= fromPathPiece) <$> lookupGetParam "from"
  entries :: [Entity CIS2Entry] <- runDB $ do
    E.select $ E.from $ \e -> do
      -- Filter by address
      E.where_ (e E.^. CIS2EntryIndex E.==. E.val (ContractIndex index))
      E.where_ (e E.^. CIS2EntrySubindex E.==. E.val (ContractSubindex subindex))
      -- sort ascending
      E.orderBy [E.asc (e E.^. CIS2EntryId)]
      case mfrom of
        Just from -> E.where_ (e E.^. CIS2EntryId E.>. E.val from)
        Nothing -> return ()
      -- Limit the number of returned rows for DOS protection
      E.limit limit
      return e
  let makeJsonEntries = map (\(Entity key CIS2Entry{..}) ->
                               object ["token" .= cIS2EntryToken_id
                                      -- amounts are always integers, so we just take the numerator
                                      -- since the integers can be very large we make them into a string
                                      ,"totalSupply" .= show (numerator cIS2EntryTotal_supply)
                                      ,"id" .= key
                                      ]
                              )
  sendResponse $ object $ [
    "limit" .= limit,
    "count" .= length entries,
    "tokens" .= makeJsonEntries entries
    ] <> maybeToList (("from" .=) <$> mfrom)


-- |Lookup token ids from the tokenId query parameter, and attempt to parse
-- them as a comma-separated list. Responds with an invalid request error
-- in case parsing is unsuccessful.
parseCIS2TokenIds :: Handler [TokenId]
parseCIS2TokenIds = do
  param <- lookupGetParam "tokenId" >>= \case
    Nothing -> respond400Error EMMissingParameter RequestInvalid
    Just p -> return p
  case mapM (AE.fromJSON . AE.String) . Text.split (== ',') $ param of
    AE.Error err -> respond400Error (EMParseError err) RequestInvalid
    AE.Success tids
        | length tids > fromIntegral (maxBound :: Word16) ->
            respond400Error (EMParseError "Too many token ids.") RequestInvalid
        | otherwise -> do
            $(logOther "Trace") (Text.pack ("Query with token ids: " ++ show tids))
            return tids

getCIS2TokenMetadata :: Word64 -> Word64 -> Handler TypedContent
getCIS2TokenMetadata index subindex = do
    let nrg = Energy 500_000 -- ~500ms worth of
    let contractAddr = ContractAddress (ContractIndex index) (ContractSubindex subindex)
    tids <- parseCIS2TokenIds
    let serializedParam = Wasm.Parameter . BSS.toShort . S.runPut $ do
            S.putWord16le (fromIntegral (length tids))
            mapM_ S.put tids
    cis2InvokeHelper contractAddr (Wasm.EntrypointName "tokenMetadata") serializedParam nrg $ \name rv -> do
        let getURLs = do
                len <- S.getWord16le
                replicateM (fromIntegral len) getMetadataUrl
        case S.runGet getURLs rv of
            Left err -> do
                $logDebug $ "Failed to parse the response from tokenMetadata: " <> Text.pack err
                respond400Error EMInvokeFailed RequestInvalid
            Right urls ->
                sendResponse $
                    object [
                        "contractName" .= name,
                        "metadata" .= AE.toJSON
                        ( zipWith
                            ( \tid md ->
                                object
                                    [ "tokenId" .= tid
                                    , "metadataURL" .= muURL md
                                    , "metadataChecksum" .= muChecksum md
                                    ]
                            )
                            tids
                            urls
                        )
                        ]

getCIS2TokenBalance :: Word64 -> Word64 -> Text -> Handler TypedContent
getCIS2TokenBalance index subindex addrText = do
    let nrg = Energy 500_000 -- ~500ms worth of
    case addressFromText addrText of
        Left err -> respond400Error (EMParseError err) RequestInvalid
        Right addr -> do
            tids <- parseCIS2TokenIds
            let serializedParam = Wasm.Parameter . BSS.toShort . S.runPut $ do
                    S.putWord16le (fromIntegral (length tids))
                    mapM_ S.put (zip tids (repeat (AddressAccount addr)))
            let contractAddr = ContractAddress (ContractIndex index) (ContractSubindex subindex)
            cis2InvokeHelper contractAddr (Wasm.EntrypointName "balanceOf") serializedParam nrg $ \_ rv -> do
                let getBalances = do
                        len <- S.getWord16le
                        replicateM (fromIntegral len) getTokenBalance
                case S.runGet getBalances rv of
                    Left err -> do
                        $logDebug $ "Failed to parse response from the balanceOf: " <> Text.pack err
                        respond400Error EMInvokeFailed RequestInvalid
                    Right urls ->
                        sendResponse $
                            AE.toJSON
                                ( zipWith
                                    ( \tid bal ->
                                        object
                                            [ "tokenId" .= tid
                                            , "balance" .= bal
                                            ]
                                    )
                                    tids
                                    urls
                                )

-- |Helper to handle the boilerplate common to both the metadata and balance of
-- queries. It handles getting the address of a contract, handling errors in
-- invocations, and calling the respective handlers for the specific query via
-- the continuation.
cis2InvokeHelper ::
    -- |Address of the contract to invoke.
    ContractAddress ->
    -- |Its entrypoint.
    Wasm.EntrypointName ->
    -- |The parameter to invoke
    Wasm.Parameter ->
    -- |Energy to allow for the invoke.
    Energy ->
    -- |Continuation applied to the name of the contract (without @_init@) and the return value produced by a successful result.
    (Text -> BS8.ByteString -> Handler TypedContent) ->
    HandlerFor Proxy TypedContent
cis2InvokeHelper contractAddr entrypoint serializedParam nrg k = do
    let invokeContext contractName =
            InvokeContract.ContractContext
                { ccInvoker = Nothing
                , ccContract = contractAddr
                , ccAmount = 0
                , ccMethod = Wasm.uncheckedMakeReceiveName contractName entrypoint
                , ccParameter = serializedParam
                , ccEnergy = nrg
                }
    let parseInstance Null = return Nothing
        parseInstance v = flip (AE.withObject "ContractInfo") v $ \obj -> do
          n <- obj .: "name"
          return (Just n)
    -- Query the name of a contract at the given block hash.
    let queryContractName block = do
          ci <- getInstanceInfo (Text.decodeUtf8 . BSL.toStrict . AE.encode $ contractAddr) block
          case ci of
            Left err -> return (Left err)
            Right v -> case parseEither parseInstance (grpcResponseVal v) of
              Left err -> return (Left err)
              Right mci -> return (Right (mci <$ v))
    let query = do
            withLastFinalBlockHash Nothing $ \bh -> do
                name <- queryContractName bh
                case name of
                    Left err -> return (Left err)
                    Right v@GRPCResponse{grpcResponseVal = Nothing} -> return (Right (Nothing <$ v))
                    Right (GRPCResponse{grpcResponseVal = Just n}) -> do
                        let ctx = invokeContext n
                        let invokeContextArg = Text.decodeUtf8 . BSL.toStrict . AE.encode $ ctx
                        res <- invokeContract invokeContextArg bh
                        case res of
                            Left err -> return (Left err)
                            Right v -> case AE.fromJSON (grpcResponseVal v) of
                                AE.Error jsonErr -> return (Left jsonErr)
                                AE.Success ir -> return (Right (Just (Wasm.initContractName n, ir) <$ v))
    runGRPC query $ \case
        Nothing -> respond404Error $ EMErrorResponse NotFound
        Just (_, InvokeContract.Failure{..}) -> do
            $logDebug $ "Invoke failed: " <> Text.pack (show rcrReason)
            respond400Error EMInvokeFailed RequestInvalid
        Just (name, InvokeContract.Success{..}) -> do
            case rcrReturnValue of
                Nothing -> respond400Error EMV0Contract RequestInvalid
                Just rv -> k name rv

-- |Balance of a CIS2 token. 
newtype TokenBalance = TokenBalance Integer
    deriving(Show)

instance AE.ToJSON TokenBalance where
  toJSON (TokenBalance i) = toJSON (show i)

-- |A custom parser for a CIS2 token that uses LEB128 to parse the token
-- balance.
getTokenBalance :: S.Get TokenBalance
getTokenBalance = TokenBalance <$> go 0 0
  where
    go !acc !s
        | s >= 37 = fail "Invalid token amount encoding."
        | otherwise = do
            n <- S.getWord8
            if testBit n 7
                then go (acc + (toInteger (clearBit n 7) `shiftL` (s * 7))) (s + 1)
                else return $! (acc + toInteger (n `shiftL` (s * 7)))

newtype Checksum = Checksum Hash
    deriving (Show, AE.ToJSON, AE.FromJSON, S.Serialize)

-- |CIS2 token metadata URL.
data MetadataURL = MetadataURL {
  -- |Metadata URL.
  muURL :: !Text,
  -- |Optional checksum (sha256) of the contents of the metadata URL.
  muChecksum :: Maybe Hash
  }

getMetadataUrl :: S.Get MetadataURL
getMetadataUrl = do
  urlLen <- S.getWord16le
  muURL' <- Text.decodeUtf8' <$> S.getByteString (fromIntegral urlLen)
  case muURL' of
    Left err -> fail (show err)
    Right muURL -> do
      muChecksum <- getMaybe S.get
      return MetadataURL{..}

-- |List transactions for the account.
getAccountTransactionsWorker :: IncludeMemos -> Text -> Handler TypedContent
getAccountTransactionsWorker includeMemos addrText = do
  i <- internationalize
  -- Get the canonical address of the account.
  -- This fails only if we cannot get an understandable response from the GRPC, i.e., if the node is not reachable,
  -- or if it returns invalid JSON.
  let getAddress k =
        case addressFromText addrText of
          Left _ -> respond400Error EMMalformedAddress RequestInvalid
          Right givenAddr -> do
            let doGetAccAddress = do
                  lastFinBlock <- getLastFinalBlockHash
                  ai <- getAccountInfo addrText lastFinBlock
                  case ai of
                    Right (GRPCResponse hds Null) -> return $ Right $ GRPCResponse hds givenAddr -- the account does not exist on the node, assume the given address is the one we want.
                    Right (GRPCResponse hds val) -> return $ Right $ GRPCResponse hds (fromRight givenAddr (parseEither (withObject "account info" (.: "accountAddress")) val))
                    -- This should not happen, accountInfo always returns valid JSON
                    Left err -> return $ Left err
            runGRPC doGetAccAddress k
  getAddress $ \addr -> do
      order <- lookupGetParam "order"
      let (ordering, ordType :: Text, ordRel) = case order of
                        Just (Text.unpack . Text.toLower -> ('d':_)) -> (E.desc, "descending", (E.<.))
                        _ -> (E.asc, "ascending", (E.>.))
      startId :: Maybe EntryId <- (>>= fromPathPiece) <$> lookupGetParam "from"
      limit <- maybe 20 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"

      -- Exclude any transactions with block time earlier than `blockTimeFrom` (seconds after epoch)
      maybeTimeFromFilter <- lookupGetParam "blockTimeFrom" <&> \case
            Nothing -> Just $ const $ return () -- the default: exclude nothing.
            Just fromTime ->
              case readMaybe $ Text.unpack fromTime of
                Nothing -> Nothing
                Just seconds -> Just $ \s ->
                  E.where_ (s E.^. SummaryTimestamp E.>=. (E.val Timestamp{tsMillis = seconds * 1000}))

      -- Exclude any transactions with block time later than `blockTimeTo` (seconds after epoch)
      maybeTimeToFilter <- lookupGetParam "blockTimeTo" <&> \case
            Nothing -> Just $ const $ return () -- the default: exclude nothing.
            Just toTime ->
              case readMaybe $ Text.unpack toTime of
                Nothing -> Nothing
                Just seconds -> Just $ \s ->
                  E.where_ (s E.^. SummaryTimestamp E.<=. (E.val Timestamp{tsMillis = seconds * 1000}))

      -- Construct filters to only query the relevant transaction types specified by `blockRewards`, `finalizationRewards`,
      -- `bakingRewards`, and `onlyEncrypted`.
      -- This is done as part of the SQL query since it is both more efficient, but also simpler since we do not have to filter
      -- on the client side.
      -- In these filters we make use of the `veryUnsafeCoerceSqlExprValue` which we really do not need,
      -- but I cannot find any API in Esqueleto that would allow us to transform from AE.Value to EJ.JSONB Value
      -- even though this should be possible.
      -- This function should either be fixed to use the Persistent abstractions without Esqueleto, the database schema type
      -- should be changed to use JSONB, or the relevant compatibility function should be added to Esqueleto.
      -- Because we are pressed for time I have the solution at the moment.
      let coerced = \s -> E.just (EInternal.veryUnsafeCoerceSqlExprValue (s E.^. SummarySummary))
      -- let isAccountTransaction = \s -> E.not_ $ coerced s EJ.?. "Right"  -- Left are account transactions.
      let isAccountTransaction = \s -> coerced s EJ.?. "Left"  -- Left are account transactions.
      let extractedTag = \s -> coerced s EJ.#>>. ["Right", "tag"] -- the reward tag.

      maybeTypeFilter <- lookupGetParam "includeRewards" <&> \case
        Nothing -> Just $ const $ return () -- the default
        Just "all" -> Just $ const $ return ()
          -- check if
          -- - either the transaction is an account transaction
          -- - or if not check that it is not a finalization reward.
        Just "allButFinalization" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "FinalizationRewards"))
        Just "none" -> Just $ \s -> E.where_ $ isAccountTransaction s
        Just _ -> Nothing

      maybeBlockRewardFilter <- lookupGetParam "blockRewards" <&> \case
        Nothing -> Just $ const $ return () -- the default: do not exclude block rewards.
        Just "y" -> Just $ const $ return ()
        -- check if
        -- - either the transaction is an account transaction
        -- - or if not check that it is not a block reward.
        Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "BlockReward"))
        Just _ -> Nothing

      maybeFinalizationRewardFilter <- lookupGetParam "finalizationRewards" <&> \case
        Nothing -> Just $ const $ return () -- the default: do not exclude finalization rewards.
        Just "y" -> Just $ const $ return ()
        -- check if
        -- - either the transaction is an account transaction
        -- - or if not check that it is not a finalization reward.
        Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "FinalizationRewards"))
        Just _ -> Nothing

      maybeBakingRewardFilter <- lookupGetParam "bakingRewards" <&> \case
        Nothing -> Just $ const $ return () -- the default: do not exclude baking rewards.
        Just "y" -> Just $ const $ return ()
        -- check if
        -- - either the transaction is an account transaction
        -- - or if not check that it is not a baking reward.
        Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "BakingRewards"))
        Just _ -> Nothing

      maybeEncryptedFilter <- lookupGetParam "onlyEncrypted" <&> \ case
        Nothing -> Just $ const $ return () -- the default: include all transactions.
        Just "n" -> Just $ const $ return ()
        Just "y" -> Just $ \s ->
          let extractedType = coerced s EJ.#>>. ["Left", "type", "contents"] -- the transaction type.
          -- check if the transaction is encrypting, decrypting, or transferring an encrypted amount.
          in E.where_ $ extractedType E.==. E.val (Just "encryptedAmountTransfer") E.||.
                        extractedType E.==. E.val (Just "transferToEncrypted") E.||.
                        extractedType E.==. E.val (Just "transferToPublic") E.||.
                        extractedType E.==. E.val (Just "encryptedAmountTransferWithMemo")
        Just _ -> Nothing

      rawReason <- isJust <$> lookupGetParam "includeRawRejectReason"
      case (maybeTimeFromFilter, maybeTimeToFilter, maybeBlockRewardFilter, maybeFinalizationRewardFilter, maybeBakingRewardFilter, maybeEncryptedFilter, maybeTypeFilter) of
        (Nothing, _, _, _, _, _, _) -> respond400Error (EMParseError "Unsupported 'blockTimeFrom' parameter.") RequestInvalid
        (_, Nothing, _, _, _, _, _) -> respond400Error (EMParseError "Unsupported 'blockTimeTo' parameter.") RequestInvalid
        (_, _, Nothing, _, _, _, _) -> respond400Error (EMParseError "Unsupported 'blockReward' parameter.") RequestInvalid
        (_, _, _, Nothing, _, _, _) -> respond400Error (EMParseError "Unsupported 'finalizationReward' parameter.") RequestInvalid
        (_, _, _, _, Nothing, _, _) -> respond400Error (EMParseError "Unsupported 'bakingReward' parameter.") RequestInvalid
        (_, _, _, _, _, Nothing, _) -> respond400Error (EMParseError "Unsupported 'onlyEncrypted' parameter.") RequestInvalid
        (_, _, _, _, _, _, Nothing) -> respond400Error (EMParseError "Unsupported 'includeRewards' parameter.") RequestInvalid
        (Just timeFromFilter, Just timeToFilter, Just blockRewardFilter, Just finalizationRewardFilter, Just bakingRewardFilter, Just encryptedFilter, Just typeFilter) -> do
          entries :: [(Entity Entry, Entity Summary)] <- runDB $ do
            E.select $ E.from $ \(e `E.InnerJoin` s) ->  do
              -- Assert join
              E.on (e E.^. EntrySummary E.==. s E.^. SummaryId)
              -- Filter by address
              E.where_ (e E.^. EntryAccount E.==. E.val (ByteStringSerialized addr))
              -- If specified, start from the given starting id
              maybe
                (return ())
                (\sid -> E.where_ (e E.^. EntryId `ordRel` E.val sid))
                startId
              -- Apply any additional filters
              timeFromFilter s
              timeToFilter s
              blockRewardFilter s
              finalizationRewardFilter s
              bakingRewardFilter s
              encryptedFilter s
              typeFilter s
              -- sort with the requested method or ascending over EntryId.
              E.orderBy [ordering (e E.^. EntryId)]
              -- Limit the number of returned rows
              E.limit limit
              return (e, s)
          case mapM (formatEntry includeMemos rawReason i addr) entries of
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
formatEntry :: IncludeMemos -- ^Whether to include memos in the enties. If not,
                           -- then transfers with memos are mapped to
                           -- corresponding transfers without memos.
            -> Bool -- ^ Whether to include a raw reject reason for account transactions or not.
            -> I18n -- ^ Internationalization of messages.
            -> AccountAddress -- ^ Address of the account whose transactions we are formatting.
            -> (Entity Entry, Entity Summary) -- ^ Database entry to be formatted.
            -> Either String Value
formatEntry includeMemos rawRejectReason i self (Entity key Entry{}, Entity _ Summary{..}) = do
  let blockDetails = ["blockHash" .= unBSS summaryBlock,
                      "blockTime" .= timestampToFracSeconds summaryTimestamp
                     ]
  transactionDetails <- case AE.fromJSON summarySummary of
    AE.Error e -> Left e
    AE.Success (Right v@BakingRewards{..}) ->
      return [
      "origin" .= object ["type" .= ("reward" :: Text)],
        -- This lookup is correct in presence of aliases since rewards are given to canonical addresses.
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
        -- This lookup is correct in presence of aliases since rewards are given to canonical addresses.
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
        "total" .= if sameAccount self stoBaker && sameAccount self stoFoundationAccount then stoBakerReward + stoFoundationCharge
                   else if sameAccount self stoBaker then stoBakerReward
                   else stoFoundationCharge, -- due to the way we index, that is the only remaining option
        "details" .= object [
          "type" .= ("blockReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
          ]
      ]
    AE.Success (Right v@PaydayFoundationReward{..}) ->
      return [
        "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= stoDevelopmentCharge,
        "details" .= object [
            "type" .= ("paydayFoundationReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
        ]
      ]
    AE.Success (Right v@PaydayAccountReward{..}) ->
      return [
        "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= (stoTransactionFees + stoBakerReward + stoFinalizationReward),
        "details" .= object [
            "type" .= ("paydayAccountReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
        ]
      ]
    AE.Success (Right v@BlockAccrueReward{}) ->
      return [
        "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= (0 :: Amount), -- Zero, since this is not a payment to a specific account
        "details" .= object [
            "type" .= ("blockAccrueReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
        ]
      ]
    AE.Success (Right v@PaydayPoolReward{}) ->
      return [
        "origin" .= object ["type" .= ("reward" :: Text)],
        "total" .= (0 :: Amount), -- Zero, since this is not a payment to a specific account
        "details" .= object [
            "type" .= ("paydayPoolReward" :: Text),
            "outcome" .= ("success" :: Text),
            "description" .= i18n i (ShortDescription v),
            "events" .= [i18n i v]
        ]
      ]
    AE.Success (Left TransactionSummary{..}) -> do
      let addMemo mmemo ps =
            case includeMemos of
              ExcludeMemo -> ps
              IncludeMemo -> case mmemo of
                [TransferMemo{..}] -> ("memo" .= tmMemo):ps
                _ -> ps
      let (origin, selfOrigin) = case tsSender of
                                   Just sender
                                     | sameAccount sender self -> (object ["type" .= ("self" :: Text)], True)
                                     | otherwise -> (object ["type" .= ("account" :: Text), "address" .= sender], False)
                                   Nothing -> (object ["type" .= ("none" :: Text)], False)

          -- If ExcludeMemo then we filter out the TransferMemo event from the
          -- list of events in order to maintain backwards compatibility.
          filterTransferMemo x = includeMemos == IncludeMemo || case x of
            TransferMemo{} -> False
            _ -> True
          (resultDetails, subtotal) = case tsResult of
            TxSuccess evts -> ((["outcome" .= ("success" :: Text), "events" .= (fmap (i18n i) . filter filterTransferMemo $ evts)]
                                <> case (tsType, evts) of
                                     (viewTransfer -> True, Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr):mmemo) ->
                                       addMemo mmemo [
                                         "transferSource" .= fromAddr,
                                         "transferDestination" .= toAddr,
                                         "transferAmount" .= amt]
                                     (viewEncryptedTransfer -> True, EncryptedAmountsRemoved{..}:NewEncryptedAmount{..}:mmemo) ->
                                       addMemo mmemo [
                                         "transferSource" .= earAccount,
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
                                     (viewScheduledTransfer -> True, TransferredWithSchedule{..}:mmemo) ->
                                       addMemo mmemo [
                                         "transferDestination" .= etwsTo,
                                         "transferAmount" .= foldl' (+) 0 (map snd etwsAmount)]
                                     (TSTAccountTransaction (Just TTRegisterData), [DataRegistered{..}]) ->
                                       ["registeredData" .= drData]
                                     _ -> []), eventSubtotal self evts )
            TxReject reason ->
              let rawReason = if rawRejectReason then ["rawRejectReason" .= reason] else []
              in (["outcome" .= ("reject" :: Text), "rejectReason" .= i18n i reason] ++ rawReason, Nothing)

          details = case includeMemos of
            IncludeMemo -> object $ ["type" .= renderTransactionSummaryType tsType, "description" .= i18n i tsType] <> resultDetails
            ExcludeMemo -> object $ ["type" .= renderTransactionSummaryType (forgetMemoInSummary tsType), "description" .= i18n i tsType] <> resultDetails

          costs
            | selfOrigin = case subtotal of
                Nothing -> let total = - toInteger tsCost in ["cost" .= show (toInteger tsCost), "total" .= show total]
                Just st -> let total = st - toInteger tsCost
                          in ["subtotal" .= show st, "cost" .= show (toInteger tsCost), "total" .= show total]
            | otherwise = ["total" .= show (fromMaybe 0 subtotal)]

          encryptedCost = case tsSender of
            Just sender
              | sameAccount sender self -> case (tsType, tsResult) of
                  (viewEncryptedTransfer -> True, TxSuccess (EncryptedAmountsRemoved{..}:NewEncryptedAmount{..}:_)) ->
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
                  (viewEncryptedTransfer -> True, TxSuccess (EncryptedAmountsRemoved{}:NewEncryptedAmount{..}:_)) ->
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

-- |Check whether the two addresses point to the same account. In protocol
-- versions 1 and 2 this should just be account address equality, and in
-- protocol version 3 it should technically be checking on the chain as well
-- since in principle there might be accounts which clash on the first 29 bytes.
-- But that will not happen in practice and if it does before we update to P3 we
-- can update the proxy with a more expensive and complex check. After we have
-- successfully updated to P3 and there have been no clashes, there will be no
-- further possible clashes.
sameAccount :: AccountAddress -> AccountAddress -> Bool
sameAccount a1 a2 = accountAddressEmbed a1 == accountAddressEmbed a2

renderTransactionType :: TransactionType -> Text
renderTransactionType TTDeployModule = "deployModule"
renderTransactionType TTInitContract = "initContract"
renderTransactionType TTUpdate = "update"
renderTransactionType TTUpdateBakerStake = "updateBakerStake"
renderTransactionType TTUpdateBakerKeys = "updateBakerKeys"
renderTransactionType TTUpdateBakerRestakeEarnings = "updateBakerRestakeEarnings"
renderTransactionType TTTransfer = "transfer"
renderTransactionType TTTransferWithMemo = "transferWithMemo"
renderTransactionType TTAddBaker = "addBaker"
renderTransactionType TTRemoveBaker = "removeBaker"
renderTransactionType TTUpdateCredentialKeys = "updateAccountKeys"
renderTransactionType TTEncryptedAmountTransfer = "encryptedAmountTransfer"
renderTransactionType TTEncryptedAmountTransferWithMemo = "encryptedAmountTransferWithMemo"
renderTransactionType TTTransferToEncrypted = "transferToEncrypted"
renderTransactionType TTTransferToPublic = "transferToPublic"
renderTransactionType TTTransferWithSchedule = "transferWithSchedule"
renderTransactionType TTTransferWithScheduleAndMemo = "transferWithScheduleAndMemo"
renderTransactionType TTUpdateCredentials = "updateCredentials"
renderTransactionType TTRegisterData = "registerData"
renderTransactionType TTConfigureBaker = "configureBaker"
renderTransactionType TTConfigureDelegation = "configureDelegation"

renderTransactionSummaryType :: TransactionSummaryType -> Text
renderTransactionSummaryType (TSTAccountTransaction (Just tt)) = renderTransactionType tt
renderTransactionSummaryType (TSTAccountTransaction Nothing) = "Malformed account transaction"
renderTransactionSummaryType (TSTCredentialDeploymentTransaction _) = "deployCredential"
renderTransactionSummaryType (TSTUpdateTransaction _) = "chainUpdate"

forgetMemoTransactionType :: TransactionType -> TransactionType
forgetMemoTransactionType TTTransferWithMemo = TTTransfer
forgetMemoTransactionType TTEncryptedAmountTransferWithMemo = TTEncryptedAmountTransfer
forgetMemoTransactionType TTTransferWithScheduleAndMemo = TTTransferWithSchedule
forgetMemoTransactionType other = other

forgetMemoInSummary :: TransactionSummaryType -> TransactionSummaryType
forgetMemoInSummary (TSTAccountTransaction (Just tt)) = TSTAccountTransaction $ Just $ forgetMemoTransactionType tt
forgetMemoInSummary other = other


eventSubtotal :: AccountAddress -> [Event] -> Maybe Integer
eventSubtotal self evts = case catMaybes $ eventCost <$> evts of
    [] -> Nothing
    l -> Just (sum l)
  where
    isSelf (AddressAccount acc) = sameAccount self acc
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
      if sameAccount self etwsFrom then -- self transfers are not possible with schedule
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
          - if the transaction nonce is finalized or the transaction is expired, delete the entry and retry from 2
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
    doGetAccInfo :: Text -> ClientMonad IO (GRPCResult (Maybe (Nonce, Amount)))
    doGetAccInfo t = do
      lastFinBlock <- getLastFinalBlockHash
      ai <- getAccountInfo t lastFinBlock
      case ai of
        Right (GRPCResponse hds Null) -> return $ Right $ GRPCResponse hds Nothing
        Right (GRPCResponse hds val) -> return $ GRPCResponse hds . Just
          <$> parseEither (withObject "account info" $ \o -> (,) <$> (o .: "accountNonce") <*> (o .: "accountAmount")) val
        Left err -> return $ Left err
    -- Determine if the transaction is or could become
    -- successfully finalized.  Returns False if the
    -- transaction is absent or is finalized but failed.
    doIsTransactionOK trHash = do
      eitherStatus <- getTransactionStatus (Text.pack $ show trHash)
      return $
        eitherStatus >>= \case
          GRPCResponse hds Null -> return $ GRPCResponse hds False
          GRPCResponse hds (Object o) -> parseEither (.: "status") o >>= \case
            "received" -> return $ GRPCResponse hds True
            "finalized" -> HM.toList <$> parseEither (.: "outcomes") o >>= \case
              [(_::BlockHash,TransactionSummary{..})] ->
                case tsResult of
                  TxSuccess{} -> return $ GRPCResponse hds True
                  TxReject{} -> return $ GRPCResponse hds False
              _ -> throwError "expected exactly one outcome for a finalized transaction"
            "committed" -> return $ GRPCResponse hds True
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
      -- Determine if there is already a GTU drop entry for this account
      rcpRecord <- runDB $ getBy (UniqueAccount (ByteStringSerialized addr))
      case rcpRecord of
        -- If there is no entry, try the drop
        Nothing -> runGRPC (doGetNonce $ accountToText dropAccount) $ \nonce -> do
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
      where
        doGetNonce :: Text -> ClientMonad IO (GRPCResult Nonce)
        doGetNonce acc = do
          nonceM <- getNextAccountNonce acc
          return $ do
            response <- nonceM
            nonce <- parseEither
              ( withObject "nonce" $ \v -> do
                  v AE..: "nonce"
              ) $ grpcResponseVal response

            return $ GRPCResponse (grpcHeaders response) nonce

getGlobalFileR :: Handler TypedContent
getGlobalFileR = toTypedContent . globalInfo <$> getYesod

-- Queries the transaction database and the GRPC,
-- then if both succeed checks that the last final block is less than `healthTolerance` seconds old.
getHealthR :: Handler TypedContent
getHealthR =
  runGRPC doGetBlockFinalInfo $ \case
      Nothing -> do
        $(logError) $ "Could not get response from GRPC."
        sendResponse $ object $ ["healthy" .= False, "reason" .= ("Could not get response from GRPC.":: String), "version" .= showVersion version]
      Just lastFinalBlockInfo -> do
        $(logInfo) "Successfully got best block info."
        _ :: [(Entity Entry, Entity Summary)] <- runDB $ E.select $
                  E.from $ \val -> do
                  E.limit 1
                  return val
        -- get block slot time from block info object, compare with current time: reject if more than `healthTolerance` seconds old.
        case fromJSON lastFinalBlockInfo of
          Error _ -> do
            i <- internationalize
            sendResponseStatus badGateway502 $ object ["errorMessage" .= i18n i EMGRPCError, "error" .= fromEnum InternalError, "version" .= showVersion version]
          Success (bir :: BlockInfo) -> do
            currentTime <- liftIO Clock.getCurrentTime
            Proxy{..} <- getYesod
            if (Clock.diffUTCTime currentTime (biBlockSlotTime bir)) < (Clock.secondsToNominalDiffTime $ fromIntegral healthTolerance)
            then sendResponse $ object ["healthy" .= True, "lastFinalTime" .= (biBlockSlotTime bir), "version" .= showVersion version]
            else sendResponse $ object ["healthy" .= False, "reason" .= ("The last final block is too old.":: String), "lastFinalTime" .= (biBlockSlotTime bir), "version" .= showVersion version]
  where
    doGetBlockFinalInfo :: ClientMonad IO (GRPCResult (Maybe Value))
    doGetBlockFinalInfo = do
      bbi <- withLastFinalBlockHash Nothing getBlockInfo
      case bbi of
        Right (GRPCResponse hds Null) -> return $ Right $ GRPCResponse hds Nothing
        Right (GRPCResponse hds val) -> return $ Right $ GRPCResponse hds $ Just val
        Left err -> return $ Left err

getIpsR :: Handler TypedContent
getIpsR = toTypedContent . ipInfo <$> getYesod

getIpsV1R :: Handler TypedContent
getIpsV1R = toTypedContent . ipInfoV1 <$> getYesod

getTermsAndConditionsVersion :: Handler TypedContent
getTermsAndConditionsVersion = do
  mtcV <- tcVersion <$> getYesod
  mtcU <- tcUrl <$> getYesod
  case (mtcV, mtcU) of
    (Just tcV, Just tcU) -> sendResponse $ object ["version" .= tcV, "url" .= tcU]
    _ -> respond404Error $ EMErrorResponse NotFound

getBakerPoolR :: Word64 -> Handler TypedContent
getBakerPoolR bid =
    runGRPC doGetBaker $ \case
      (_, AE.Null) -> respond404Error $ EMErrorResponse NotFound
      (lf, psV) -> do
        $(logInfo) "Successfully got baker pool status."
        case AE.fromJSON psV of
          AE.Success bps@BakerPoolStatus{..} ->
            case psBakerStakePendingChange of
              PPCNoChange -> sendResponse psV
              PPCReduceBakerCapital{..} -> runGRPC (getRewardPeriodLength lf) $ \case
                Nothing -> sendResponse psV
                -- if there is a pending change we add the estimated change time to the response object.
                -- The way this is done is to patch the returned JSON Value. This is not ideal, but it seems preferrable
                -- to introducing a new type just for this.
                Just rewardEpochs -> runGRPC (getParameters lf) $ \(nextPaydayTime, epochDuration) -> do
                  let r = object [
                        "pendingChangeType" .= String "ReduceBakerCapital",
                        "bakerEquityCapital" .= ppcBakerEquityCapital,
                        "effectiveTime" .= ppcEffectiveTime,
                        "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs ppcEffectiveTime
                        ]
                  case AE.toJSON bps of
                    AE.Object o -> sendResponse (AE.toJSON (KM.insert "bakerStakePendingChange" r o))
                    _ -> sendResponse psV
              PPCRemovePool{..} -> runGRPC (getRewardPeriodLength lf) $ \case
                Nothing -> sendResponse psV
                Just rewardEpochs -> runGRPC (getParameters lf) $ \(nextPaydayTime, epochDuration) -> do
                  let r = object [
                        "pendingChangeType" .= String "RemovePool",
                        "effectiveTime" .= ppcEffectiveTime,
                        "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs ppcEffectiveTime
                        ]
                  case AE.toJSON bps of
                    AE.Object o -> sendResponse (AE.toJSON (KM.insert "bakerStakePendingChange" r o))
                    _ -> sendResponse psV
          AE.Success PassiveDelegationStatus{} -> sendResponse psV
          AE.Error err -> do
            $(logError) $ "Could not parse pool status response: " <> Text.pack err
            i <- internationalize
            sendResponseStatus badGateway502 $ object [
              "errorMessage" .= i18n i EMGRPCError,
              "error" .= fromEnum InternalError
              ]
  where
    doGetBaker :: ClientMonad IO (GRPCResult (Text, Value))
    doGetBaker = withLastFinalBlockHash Nothing (\lf ->
      fmap (\x -> GRPCResponse (grpcHeaders x) $ (lf,) $ grpcResponseVal x)
        <$> getPoolStatus (BakerId $ AccountIndex bid) False lf)
    doGetNextPayday :: Text -> ClientMonad IO (GRPCResult UTCTime)
    doGetNextPayday lastFinal = do
      rewardStatus <- getRewardStatus lastFinal
      return $ do
        response <- rewardStatus
        utc <- parseEither
                    ( withObject "Best finalized block" $ \v -> do
                        v AE..: "nextPaydayTime"
                    ) $ grpcResponseVal response
        return $ GRPCResponse (grpcHeaders response) utc
    doGetEpochLength :: ClientMonad IO (GRPCResult Duration)
    doGetEpochLength = do
      consensusStatus <- getConsensusStatus
      return $ do
        response <- consensusStatus
        dur <- parseEither
                    ( withObject "Consensus status" $ \v -> do
                        v AE..: "epochDuration"
                    ) $ grpcResponseVal response
        Right $ GRPCResponse (grpcHeaders response) dur
    getParameters :: Text -> ClientMonad IO (GRPCResult (UTCTime, Duration))
    getParameters lastFinal = do
      nextPayday <- doGetNextPayday lastFinal
      epochDuration <- doGetEpochLength
      return $ do
        GRPCResponse hds l <- nextPayday
        GRPCResponse hds' r <- epochDuration
        return $ GRPCResponse (hds ++ hds') (l, r)

getChainParametersR :: Handler TypedContent
getChainParametersR =
    runGRPC doGetParameters $ \(v :: Value) -> do
      $(logInfo) "Successfully got chain parameters."
      sendResponse v
  where
    doGetParameters = do
      summary <- withLastFinalBlockHash Nothing getBlockSummary
      return $ do
        response <- summary
        val <- parseEither
                    ( withObject "Best finalized block" $ \v -> do
                        updates <- v AE..: "updates"
                        updates AE..: "chainParameters"
                    ) . grpcResponseVal
                    =<< summary

        Right $ GRPCResponse (grpcHeaders response) val

getNextPaydayR :: Handler TypedContent
getNextPaydayR =
    runGRPC doGetParameters $ \(v :: UTCTime) -> do
      let timestampObject = object ["nextPaydayTime" .= v]
      $(logInfo) "Successfully got next payday."
      sendResponse $ toJSON timestampObject
  where
    doGetParameters = do
      rewardStatus <- withLastFinalBlockHash Nothing getRewardStatus
      return $ do
        response <- rewardStatus
        utc <- parseEither
          ( withObject "Best finalized block" $ \v -> do
              v AE..: "nextPaydayTime"
          ) $ grpcResponseVal response
        Right $ GRPCResponse (grpcHeaders response) utc

getPassiveDelegationR :: Handler TypedContent
getPassiveDelegationR =
    runGRPC doGetPassiveDelegation $ \v -> do
      $(logInfo) "Successfully got baker pool status."
      sendResponse v
  where
    doGetPassiveDelegation = withLastFinalBlockHash Nothing (getPoolStatus (BakerId $ AccountIndex 0) True)

matchesVersion :: Word -> Maybe ForcedUpdateConfig -> AE.Value
matchesVersion _ Nothing = AE.object ["status" AE..= String "ok"]
matchesVersion queryVersion (Just ForcedUpdateConfig{..})
  | inRanges fucForceUpdate queryVersion = AE.object [
      "status" AE..= String "needsUpdate",
      "url" AE..= fucURL
    ]
  | inRanges fucSuggestUpdate queryVersion = AE.object [
      "status" AE..= String "warning",
      "url" AE..= fucSuggestURL
      ]
  | otherwise = AE.object ["status" AE..= String "ok"]

getAppSettingsV0 :: Handler TypedContent
getAppSettingsV0 = do
  Proxy{..} <- getYesod
  getAppSettingsWorker forcedUpdateConfigAndroidV0 forcedUpdateConfigIOSV0

getAppSettingsV1 :: Handler TypedContent
getAppSettingsV1 = do
  Proxy{..} <- getYesod
  getAppSettingsWorker forcedUpdateConfigAndroidV1 forcedUpdateConfigIOSV1

getAppSettingsWorker ::
  -- |Forced update configuration for the Android variant of the app.
  Maybe ForcedUpdateConfig ->
  -- |Forced update configuration for the IOS variant of the app.
  Maybe ForcedUpdateConfig ->
  Handler TypedContent
getAppSettingsWorker fucAndroid fucIOS = do
  mPlatform <- lookupGetParam "platform"
  mVersion <- lookupGetParam "appVersion"
  case (mPlatform, readMaybe . Text.unpack =<< mVersion) of
    (Just platform, Just queryVersion) -> do
      if platform == "android" then
        sendResponse (matchesVersion queryVersion fucAndroid)
      else if platform == "ios" then
        sendResponse (matchesVersion queryVersion fucIOS)
      else
        respond400Error (EMParseError "'param' should either be 'android' or 'ios'") RequestInvalid
    (Just _, Nothing) -> respond400Error (EMParseError "'version' parameter is not present or readable. It must be a non-negative integer.") RequestInvalid
    (Nothing, Just _) -> respond400Error (EMParseError "'platform' field is not present.") RequestInvalid
    (Nothing, Nothing) -> respond400Error (EMParseError "'platform' field is not present and 'version' field is either not present or not readable.") RequestInvalid

getEpochLengthR :: Handler TypedContent
getEpochLengthR =
    runGRPC doGetEpochLength $ \(v :: Duration) -> do
      let epochLengthObject = object ["epochLength" .= v]
      $(logInfo) "Successfully got epoch length."
      sendResponse $ toJSON epochLengthObject
  where
    doGetEpochLength = do
      consensusStatus <- getConsensusStatus
      return $ do
        response <- consensusStatus
        dur <- parseEither
                    ( withObject "Consensus status" $ \v -> do
                        v AE..: "epochDuration"
                    ) $ grpcResponseVal response
        Right $ GRPCResponse (grpcHeaders response) dur
