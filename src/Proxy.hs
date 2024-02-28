{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Proxy where

import Database.Persist.Postgresql
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH

import Control.Applicative
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import qualified Data.Char as CH
import Data.Ratio

import Control.Arrow (first, left)
import Control.Exception (SomeException, catch)
import Control.Monad
import Data.Aeson (Result (..), fromJSON, withObject)
import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Parser (json')
import Data.Aeson.Types (Pair, parse)
import Data.Conduit (connect)
import Data.Conduit.Attoparsec (sinkParserEither)
import Data.Foldable
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import qualified Data.Proxy as Proxy
import Data.Range
import qualified Data.Ratio as Rational
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Version (showVersion)
import Data.Word
import qualified Database.Esqueleto.Internal.Internal as EInternal
import qualified Database.Esqueleto.Legacy as E
import qualified Database.Esqueleto.PostgreSQL.JSON as EJ
import Lens.Micro.Platform hiding ((.=))
import Network.GRPC.HTTP2.Types (GRPCStatusCode (..))
import Network.HTTP.Types (Status, badGateway502, badRequest400, internalServerError500, notFound404, serviceUnavailable503)
import System.Random
import Text.Read hiding (String)
import Web.Cookie
import Yesod hiding (InternalError)
import qualified Yesod

import Data.Time.Clock as Clock

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Block
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Types.Parameters
import Concordium.Types.Queries hiding (Summary)
import Concordium.Types.Transactions
import Concordium.Utils.Serialization (getMaybe)
import qualified Concordium.Wasm as Wasm
import Paths_wallet_proxy (version)

import Concordium.Client.GRPC2
import Concordium.Client.Runner.Helper
import Concordium.Client.Types.Transaction (
    accountDecryptEnergyCost,
    accountDecryptPayloadSize,
    accountEncryptEnergyCost,
    accountEncryptPayloadSize,
    bakerConfigureEnergyCostWithKeys,
    bakerConfigureEnergyCostWithoutKeys,
    bakerConfigurePayloadSize,
    delegationConfigureEnergyCost,
    encryptedTransferEnergyCost,
    encryptedTransferPayloadSize,
    minimumCost,
    registerDelegationPayloadSize,
    removeDelegationPayloadSize,
    simpleTransferEnergyCost,
    simpleTransferPayloadSize,
    transferWithScheduleEnergyCost,
    transferWithSchedulePayloadSize,
    updateDelegationPayloadSize,
 )
import Concordium.Common.Version
import Concordium.Crypto.ByteStringHelpers (ByteStringHex (..))
import Concordium.Crypto.SHA256 (Hash)
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.ID.Types (CredentialIndex, KeyIndex, addressFromText)
import qualified Logging

import Internationalization

-- | Wraps a type for persistent storage via a serialization to a 'ByteString'.
newtype ByteStringSerialized a = ByteStringSerialized {unBSS :: a}
    deriving newtype (S.Serialize, Eq, Ord, Show)

instance (S.Serialize a) => PersistField (ByteStringSerialized a) where
    toPersistValue = toPersistValue . S.encode
    fromPersistValue =
        fromPersistValue >=> left (Text.pack) . S.decode

instance (S.Serialize a) => PersistFieldSql (ByteStringSerialized a) where
    sqlType _ = sqlType (Proxy.Proxy :: Proxy.Proxy BS.ByteString)

newtype TokenId = TokenId {unTokenId :: BSS.ShortByteString}
    deriving (Eq, Show)
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

-- | Create the database schema and types. This creates a type called @Summary@
--  with fields @summaryBlock@, @summaryTimestamp@, etc., with stated types.
--  Analogously for @Entry@ and @ContractEntry@.
--  It also generates functionality for retrieving these records from SQL rows.
--  This is used below when querying the database (e.g., Entity Summary).
share
    [mkPersist sqlSettings]
    [persistLowerCase|
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

data ErrorCode = InternalError | RequestInvalid | DataNotFound | Unavailable
    deriving (Eq, Show, Enum)

-- | Configuration for the @appSettings@ endpoint that returns whether the app is
--  out of date or not.
data ForcedUpdateConfig = ForcedUpdateConfig
    { -- | Versions which are forced to update.
      fucForceUpdate :: ![Range Word],
      -- | Versions which are going to be suggested to update.
      fucSuggestUpdate :: ![Range Word],
      -- | URL to update to if the version matches any of the above.
      fucURL :: !String,
      -- | URL to update to if the version matches the suggest update but not the
      --  forced update.
      fucSuggestURL :: !String
    }
    deriving (Show)

data Proxy = Proxy
    { grpcEnvData :: !EnvData,
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
      -- | The version of terms and conditions currently in effect.
      --  If not set the endpoint termsAndConditionsVersion is disabled.
      tcVersion :: Maybe String,
      -- | URL to access terms and conditions.
      --  If not set the endpoint termsAndConditionsVersion is disabled.
      tcUrl :: Maybe String
    }

-- | Data needed for GTU drops.
data GTUDropData = GTUDropData
    { -- | Account to send GTU from.
      dropAccount :: !AccountAddress,
      -- | Keys for the account.
      dropKeys :: [(CredentialIndex, [(KeyIndex, KeyPair)])],
      -- | Amount to drop
      dropAmount :: !Amount
    }

-- Database table for GTU drop
share
    [mkPersist sqlSettings, mkMigrate "migrateGTURecipient"]
    [persistLowerCase|
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

mkYesod
    "Proxy"
    [parseRoutes|
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
/v1/CIS2TokenMetadata/#Word64/#Word64 CIS2TokenMetadataV1 GET
/v1/CIS2TokenBalance/#Word64/#Word64/#Text CIS2TokenBalanceV1 GET
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
            Yesod.InternalError{} -> InternalError
            InvalidArgs{} -> RequestInvalid
            NotAuthenticated -> RequestInvalid
            PermissionDenied{} -> RequestInvalid
            BadMethod{} -> RequestInvalid

    shouldLogIO Proxy{..} _source level = return $ Logging.convertLogLevel level <= logLevel

-- | Terminate execution and respond with 400 status code with the given error
--  description.
respond400Error :: ErrorMessage -> ErrorCode -> Handler a
respond400Error err code = do
    i <- internationalize
    sendResponseStatus badRequest400 $
        object
            [ "errorMessage" .= i18n i err,
              "error" .= fromEnum code
            ]

-- | Terminate execution and respond with 404 status code with the given error
--  description.
respond404Error :: ErrorMessage -> Handler a
respond404Error err = do
    i <- internationalize
    sendResponseStatus notFound404 $
        object
            [ "errorMessage" .= i18n i err,
              "error" .= fromEnum DataNotFound
            ]

-- | Parse a set-cookie header string.
parseSetCookie' :: BS.ByteString -> SetCookie
parseSetCookie' c =
    let sc = parseSetCookie c
    in  sc{setCookieExpires = setCookieExpires sc <|> (lookup "expires" flags >>= parseSetCookieExpires')}
  where
    breakDiscard :: Word8 -> BS.ByteString -> (BS.ByteString, BS.ByteString)
    breakDiscard w s =
        let (x, y) = BS.break (== w) s
        in  (x, BS.drop 1 y)
    pairs = map (parsePair . dropSpace) $ BS.split 59 c ++ [BS8.empty] -- 59 = semicolon
    flags = map (first (BS8.map CH.toLower)) $ tail pairs
    parsePair = breakDiscard 61 -- equals sign
    dropSpace = BS.dropWhile (== 32) -- space

-- ^ we need this for addHeader since expiry fails to parse in
-- parseSetCookie in Web.Cookie.

-- | Parse a setcookie expires field.
parseSetCookieExpires' :: BS.ByteString -> Maybe UTCTime
parseSetCookieExpires' s = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X GMT" $ BS8.unpack s

-- | Type used to specify custom error information in responses for @runGRPCWithCustomError@.
type ResponseOnError = (ErrorType, Maybe Status, Maybe ErrorCode, Maybe ErrorMessage)

-- | Error types that can occur internally in @runGRPCWithCustomError@.
data ErrorType
    = -- | A client error occurred.
      ClientError
    | -- | The GRPC invocation succeeded with status code 'OK', but some invariant error occurred, e.g. when converting the payload.
      InvariantError
    | -- | An invalid GRPC status code was present in the response.
      StatusInvalidError
    | -- | A non-'OK' GRPC status code was present in the response.
      StatusNotOkError GRPCStatusCode
    | -- | The GRPC invocation failed.
      RequestFailedError
    deriving (Eq)

-- | Run a GRPC request.
--  Return a handler which runs a given @ClientMonad@ with return type @GRPCResult (Either String a)@, and responds with an
--  error if the returned @GRPCResult@ is not @StatusOk@, or if it is @StatusOk (Left err)@. Otherwise the return value of
--  type @a@ is mapped into a @Handler TypedContent@ under the provided callback.
--
--  Client errors are mapped to status @badGateway502@ and error code @InternalError@ and returned in the response. Otherwise,
--  the GRPC call was successful and the following mapping applies:
--
--  - @StatusOk (Left err)@ maps to status @internalServerError500@ and error code @InternalError@.
--  - @StatusInvalid@ maps to status @badGateway502@ and error code @InternalError@.
--  - @StatusNotOk (NOT_FOUND, err)@ maps to status @notFound404@ and error code @DataNotFound@.
--  - @StatusNotOk (a, err)@ maps to status @badGateway502@ and error code @InternalError@.
--  - @RequestFailed@ maps to status @badGateway502@ and error code @InternalError@.
runGRPC :: ClientMonad IO (GRPCResult (Either String a)) -> (a -> Handler TypedContent) -> Handler TypedContent
runGRPC = runGRPCWithCustomError Nothing

-- | Run a GRPC request and optionally provide a @ResponseOnError@ to override the default error responses.
--  Return a handler which runs a given @ClientMonad@ with return type @GRPCResult (Either String a)@, and responds with an
--  error if the returned @GRPCResult@ is not @StatusOk@, or if it is @StatusOk (Left err)@. Otherwise the return value of
--  type @a@ is mapped into a @Handler TypedContent@ under the provided callback.
--
--  Client errors are mapped to status @badGateway502@ and error code @InternalError@ and returned in the response by default.
--  These values can be overridden by supplying the value @ClientError@ in the supplied @ResponseOnError@. Otherwise, the
--  GRPC call was successful and the following default mapping applies:
--
--  - @StatusOk (Left err)@ maps to status @internalServerError500@ and error code @InternalError@. These can be overridden by
--    supplying @InvariantError@ in the @ResponseOnError@.
--  - @StatusInvalid@ maps to status @badGateway502@ and error code @InternalError@. These can be overridden by supplying
--    @StatusInvalidError@ in the @ResponseOnError@.
--  - @StatusNotOk (NOT_FOUND, err)@ maps to status @notFound404@ and error code @DataNotFound@. These can be overridden by
--    supplying @StatusNotOkError NOT_FOUND@ in the @ResponseOnError@.
--  - @StatusNotOk (a, err)@ maps to status @badGateway502@ and error code @InternalError@. These can be overridden by
--    supplying @StatusNotOkError a@ in the @ResponseOnError@.
--  - @RequestFailed@ maps to status @badGateway502@ and error code @InternalError@. These can be overridden by supplying
--    @RequestFailedError@ in the @ResponseOnError@.
runGRPCWithCustomError ::
    Maybe ResponseOnError ->
    -- | The @ClientMonad@ to run.
    ClientMonad IO (GRPCResult (Either String a)) ->
    -- | The handler.
    (a -> Handler c) ->
    Handler c
runGRPCWithCustomError resp c k = do
    cfg <- grpcEnvData <$> getYesod
    -- The cache stores any cookies we had seen in this request, together with
    -- the flag indicating if we already set the response set-cookie headers.
    -- The latter is necessary since for some client requests the proxy makes a
    -- number of requests to the node, each of those sends Set-Cookie headers in
    -- response and we don't want to forward all of these back to the client,
    -- since it leads to very large headers with too many duplicate set-cookie
    -- headers. The compromise we have is that we only forward the first set of
    -- cookies we get from the node request back to the client.
    (cookies, alreadySet) <- getCookies
    let
        exHandler :: SomeException -> IO (Either String a, Map.Map BS.ByteString BS.ByteString)
        exHandler e = pure (Left $ show e, Map.empty)
    $(logOther "Trace") $ "Invoking queries with headers: " <> Text.pack (show cookies)
    -- Run the client and handle any client-related exceptions.
    (clientRes, updatedCookies) <- liftIO $ fmap (\(x, y) -> (left show x, y)) (runClientWithCookies cookies cfg c) `catch` exHandler
    -- Process the result.
    responseInfoOrErrorData <- case clientRes of
        -- A client error occurred.
        Left clientError -> do
            $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack (show clientError)
            return $ Left (ClientError, badGateway502, InternalError, EMGRPCError)
        -- Otherwise the HTTP/2 request succeeded and there is a GRPC result.
        Right grpcResult -> do
            case grpcResult of
                -- GRPC response with status code 'OK'.
                StatusOk (GRPCResponse hds resultValue) -> do
                    case resultValue of
                        -- Invariant error, e.g. converting the GRPC response payload to Haskell datatype.
                        Left err -> do
                            $(logError) $ "Invariant error: " <> Text.pack err
                            return $
                                Left (InvariantError, internalServerError500, InternalError, EMErrorResponse $ Yesod.InternalError "Invariant error.")
                        -- Otherwise the conversion succeeded.
                        Right val -> do
                            $(logOther "Trace") $ "Got these response headers: " <> Text.pack (show hds)
                            -- set cookies in response
                            let scs = parseSetCookie' . snd <$> (filter $ ("set-cookie" ==) . fst) hds
                            unless alreadySet $ do
                                mapM_ setCookie scs
                                $(logOther "Trace") $ "Set-cookies headers to be included in yesod response to client: " <> Text.pack (show scs)
                            -- update cookie map
                            let !newCookies = Map.union updatedCookies cookies
                            cacheSet $! (newCookies, True)
                            return $ Right $ k val
                -- GRPC response with an invalid status code.
                StatusInvalid -> do
                    $(logError) "Got invalid GRPC status code."
                    return $
                        Left (StatusInvalidError, badGateway502, InternalError, EMGRPCErrorResponse "Invalid GRPC status code.")
                -- GRPC response with status code 'NOT_FOUND'.
                StatusNotOk (NOT_FOUND, err) -> do
                    return $
                        Left (StatusNotOkError NOT_FOUND, notFound404, DataNotFound, EMGRPCErrorResponse $ "Requested object was not found: " <> err)
                -- GRPC response with status code 'CANCELLED', i.e., the server timed out the request.
                StatusNotOk (CANCELLED, err) -> do
                    return $
                        Left (StatusNotOkError CANCELLED, serviceUnavailable503, Unavailable, EMGRPCErrorResponse $ "The node is overloaded so the request was cancelled: " <> err)
                -- GRPC response with status code 'RESOURCE_EXHAUSTED'.
                StatusNotOk (RESOURCE_EXHAUSTED, err) -> do
                    return $
                        Left (StatusNotOkError RESOURCE_EXHAUSTED, serviceUnavailable503, Unavailable, EMGRPCErrorResponse $ "The node is overloaded so the request was cancelled: " <> err)
                -- GRPC response with status code 'DEADLINE_EXCEEDED'
                StatusNotOk (DEADLINE_EXCEEDED, err) -> do
                    return $
                        Left (StatusNotOkError DEADLINE_EXCEEDED, serviceUnavailable503, Unavailable, EMGRPCErrorResponse $ "The node is overloaded so the request was cancelled: " <> err)
                -- GRPC response with valid non-'OK' status code.
                StatusNotOk (status, err) -> do
                    $(logError) $ "Got non-OK GRPC status code '" <> Text.pack (show status) <> "': " <> Text.pack err
                    return $
                        Left (StatusNotOkError status, badGateway502, InternalError, EMGRPCErrorResponse $ "Non-OK GRPC status code '" <> show status <> "'.")
                -- GRPC request failed in some other way.
                RequestFailed err -> do
                    $(logError) $ "GRPC call failed: " <> Text.pack err
                    return $
                        Left (RequestFailedError, badGateway502, InternalError, EMGRPCErrorResponse "Unable to communicate with the node.")
    case responseInfoOrErrorData of
        -- An error occurred. Send an error response, potentially overriding default values.
        Left (errType, status, errCode, errMsg) -> do
            let (status', errCode', errMsg') = case resp of
                    -- Custom error information was provided.
                    Just (onErrType, statusM, errCodeM, errMsgM) ->
                        -- If the error type matches that of the provided, override with the provided error information.
                        if errType == onErrType
                            then (fromMaybe status statusM, fromMaybe errCode errCodeM, fromMaybe errMsg errMsgM)
                            else (status, errCode, errMsg)
                    -- Otherwise, no custom error information was provided.
                    Nothing -> (status, errCode, errMsg)
            i <- internationalize
            sendResponseStatus status' $
                object
                    [ "errorMessage" .= i18n i errMsg',
                      "error" .= fromEnum errCode'
                    ]
        -- Otherwise everything went well, so we send a 200 response.
        Right h -> h
  where
    getYesodCookieMap :: Handler (Map.Map BS.ByteString BS.ByteString)
    getYesodCookieMap =
        Map.fromList
            . fmap (\(k', v') -> (Text.encodeUtf8 k', Text.encodeUtf8 v'))
            . reqCookies
            <$> getRequest
    getCookies :: Handler (Map.Map BS.ByteString BS.ByteString, Bool)
    getCookies = do
        -- map of cookies to be included in runClient
        cacheContents <- cacheGet
        -- yesod cookies in client request
        yCookieMap <- getYesodCookieMap
        case cacheContents of
            Nothing -> return (yCookieMap, False)
            Just (scm, alreadySet) -> return $ (Map.union scm yCookieMap, alreadySet)

firstPaydayAfter ::
    -- | Time of the next payday.
    UTCTime ->
    -- | Duration of an epoch
    Duration ->
    -- | Length of a payday.
    RewardPeriodLength ->
    -- | Time at which the cooldown expires.
    UTCTime ->
    UTCTime
firstPaydayAfter nextPayday epochDuration (RewardPeriodLength ep) cooldownEnd =
    if cooldownEnd <= nextPayday
        then nextPayday
        else
            let timeDiff = Clock.diffUTCTime cooldownEnd nextPayday
                paydayLength = durationToNominalDiffTime (fromIntegral ep * epochDuration)
                mult :: Word = ceiling (timeDiff / paydayLength)
            in  Clock.addUTCTime (fromIntegral mult * paydayLength) nextPayday

pendingChangeToJSON :: (AE.KeyValue kv) => Maybe UTCTime -> Duration -> Maybe RewardPeriodLength -> StakePendingChange' UTCTime -> [kv]
pendingChangeToJSON _ _ _ NoChange = []
pendingChangeToJSON mnextPaydayTime epochDuration mrewardEpochs (ReduceStake amt eff) =
    [ "pendingChange"
        .= object
            ( [ "change" .= String "ReduceStake",
                "newStake" .= amt,
                "effectiveTime" .= eff
              ]
                <> maybeToList ((\rewardEpochs nextPaydayTime -> "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs eff) <$> mrewardEpochs <*> mnextPaydayTime)
            )
    ]
pendingChangeToJSON mnextPaydayTime epochDuration mrewardEpochs (RemoveStake eff) =
    [ "pendingChange"
        .= object
            ( [ "change" .= String "RemoveStake",
                "effectiveTime" .= eff
              ]
                <> maybeToList ((\rewardEpochs nextPaydayTime -> "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs eff) <$> mrewardEpochs <*> mnextPaydayTime)
            )
    ]

getRewardPeriodLength :: (MonadFail m, MonadIO m) => BlockHash -> ClientMonad m (GRPCResult (Either String (Maybe RewardPeriodLength)))
getRewardPeriodLength lfb = do
    bcpRes <- getBlockChainParameters (Given lfb)
    case getResponseValueAndHeaders bcpRes of
        Left errRes -> return errRes
        Right (cpksRes, hds) -> do
            case cpksRes of
                Left err -> return $ StatusOk $ GRPCResponse hds $ Left err
                Right (EChainParametersAndKeys (ecpParams :: ChainParameters' cpv) _) -> do
                    let rpLength = case chainParametersVersion @cpv of
                            SChainParametersV0 -> Nothing
                            SChainParametersV1 -> Just $ ecpParams ^. cpTimeParameters . supportedOParam . tpRewardPeriodLength
                            SChainParametersV2 -> Just $ ecpParams ^. cpTimeParameters . supportedOParam . tpRewardPeriodLength
                    return $ StatusOk $ GRPCResponse hds $ Right rpLength

-- | Get the balance of an account. If successful, the result is a JSON
--  object consisting of the following optional fields:
--    * "finalizedBalance": the balance of the account at the last finalized block
--    * "currentBalance": the balance of the account at the current best block
--  If neither field is present, the account does not currently exist.
--  The "finalizedBalance" field will be absent if the account has been created since
--  the last finalized block.
--  If the "finalizedBalance" field is present, then the "currentBalance" field will
--  also be present, since accounts cannot be deleted from the chain.
getAccountBalanceR :: Text -> Handler TypedContent
getAccountBalanceR addrText = do
    accAddr <- doParseAccountAddress "getAccountBalance" addrText
    runGRPC (doGetBal accAddr) $ \case
        Nothing -> sendResponse (object []) -- send an empty object if something
        -- was not found. Some wallets rely on this behaviour instead of
        -- checking the status code for "not found".
        Just (lastFinInfo, bestInfo, nextPayday, epochDuration, lastFinBlock) -> do
            let
                getBal :: AccountInfo -> Either (Maybe Value) (Maybe RewardPeriodLength -> Value)
                getBal AccountInfo{..} = do
                    let balanceInfo =
                            [ "accountAmount" .= aiAccountAmount,
                              "accountEncryptedAmount" .= aiAccountEncryptedAmount,
                              "accountNonce" .= aiAccountNonce,
                              "accountReleaseSchedule" .= aiAccountReleaseSchedule,
                              "accountIndex" .= aiAccountIndex
                            ]
                    case aiStakingInfo of
                        AccountStakingNone -> Left . Just $ object balanceInfo
                        AccountStakingBaker{..} -> do
                            let infoWithoutPending =
                                    [ "stakedAmount" .= asiStakedAmount,
                                      "restakeEarnings" .= asiStakeEarnings,
                                      "bakerId" .= _bakerIdentity asiBakerInfo,
                                      "bakerElectionVerifyKey" .= _bakerElectionVerifyKey asiBakerInfo,
                                      "bakerSignatureVerifyKey" .= _bakerSignatureVerifyKey asiBakerInfo,
                                      "bakerAggregationVerifyKey" .= _bakerAggregationVerifyKey asiBakerInfo
                                    ]
                                        <> maybe [] (\bpi -> ["bakerPoolInfo" .= bpi]) asiPoolInfo
                            case asiPendingChange of
                                NoChange -> Left . Just $ object $ balanceInfo <> ["accountBaker" .= object infoWithoutPending]
                                _ ->
                                    let bi rpl = object $ infoWithoutPending <> pendingChangeToJSON nextPayday epochDuration rpl asiPendingChange
                                    in  Right $ \rpl -> object (balanceInfo <> ["accountBaker" .= bi rpl])
                        AccountStakingDelegated{..} -> do
                            let infoWithoutPending =
                                    [ "stakedAmount" .= asiStakedAmount,
                                      "restakeEarnings" .= asiStakeEarnings,
                                      "delegationTarget" .= asiDelegationTarget
                                    ]
                            case asiDelegationPendingChange of
                                NoChange -> Left . Just $ object $ balanceInfo <> ["accountDelegation" .= object infoWithoutPending]
                                _ ->
                                    let di rpl = object $ infoWithoutPending <> pendingChangeToJSON nextPayday epochDuration rpl asiDelegationPendingChange
                                    in  Right $ \rpl -> object (balanceInfo ++ ["accountDelegation" .= di rpl])
                lastFinBalComp = getBal lastFinInfo
                bestBalComp = getBal bestInfo
            let response lastFinBal bestBal = do
                    $(logInfo) $
                        "Retrieved account balance for "
                            <> addrText
                            <> ": finalizedBalance="
                            <> (Text.pack $ show lastFinBal)
                            <> ", currentBalance="
                            <> (Text.pack $ show bestBal)
                    sendResponse $
                        object $
                            (maybe [] (\b -> ["finalizedBalance" .= b]) lastFinBal)
                                <> (maybe [] (\b -> ["currentBalance" .= b]) bestBal)
            case (lastFinBalComp, bestBalComp) of
                (Left lastFinBal, Left bestBal) -> response lastFinBal bestBal
                (Left lastFinBal, Right bestBalF) -> runGRPC (getRewardPeriodLength lastFinBlock) $ \rpl -> response lastFinBal (Just (bestBalF rpl))
                (Right lastFinBalF, Left bestBal) -> runGRPC (getRewardPeriodLength lastFinBlock) $ \rpl -> response (Just (lastFinBalF rpl)) bestBal
                (Right lastFinBalF, Right bestBalF) -> runGRPC (getRewardPeriodLength lastFinBlock) $ \rpl -> response (Just (lastFinBalF rpl)) (Just (bestBalF rpl))
  where
    doGetBal :: AccountAddress -> ClientMonad IO (GRPCResult (Either String (Maybe (AccountInfo, AccountInfo, Maybe UTCTime, Duration, BlockHash))))
    doGetBal accAddr = do
        -- Get the consensus info, for retrieving the epoch duration.
        cInfoRes <- getConsensusInfo
        case getResponseValueAndHeaders cInfoRes of
            Left errRes -> return errRes
            Right (Left err, hds) -> return $ StatusOk $ GRPCResponse hds $ Left err
            Right (Right status, _) -> do
                -- Get the account info for the account in the last finalized block.
                let lastFinBlock = csLastFinalizedBlock status
                lastFinAccInfoRes <- getAccountInfo (AccAddress accAddr) (Given lastFinBlock)
                let onResponse resp k =
                        case resp of
                            StatusNotOk (NOT_FOUND, _) -> return $ StatusOk $ GRPCResponse [] (Right Nothing)
                            StatusNotOk err -> return $ StatusNotOk err
                            StatusInvalid -> return $ StatusInvalid
                            RequestFailed err -> return $ RequestFailed err
                            StatusOk r -> case grpcResponseVal r of
                                Left err -> return $ StatusOk $ GRPCResponse (grpcHeaders r) $ Left err
                                Right info -> k info
                onResponse lastFinAccInfoRes $ \lastFinInfo -> do
                    -- Get the account info for the account in the best block.
                    bestAccInfoRes <- getAccountInfo (AccAddress accAddr) Best
                    onResponse bestAccInfoRes $ \bestInfo -> do
                        -- Get the reward status in the last finalized block, for retrieving the next payday time.
                        rewardStatusRes <- getTokenomicsInfo (Given lastFinBlock)
                        case getResponseValueAndHeaders rewardStatusRes of
                            Left errRes -> return errRes
                            Right (Left err, hds) -> return $ StatusOk $ GRPCResponse hds $ Left err
                            Right (Right rewardStatus, rsHds) -> do
                                let epochDuration = csEpochDuration status
                                case rewardStatus of
                                    RewardStatusV0{} -> return $ StatusOk $ GRPCResponse rsHds $ Right (Just (lastFinInfo, bestInfo, Nothing, epochDuration, lastFinBlock))
                                    RewardStatusV1{..} -> return $ StatusOk $ GRPCResponse rsHds $ Right (Just (lastFinInfo, bestInfo, Just rsNextPaydayTime, epochDuration, lastFinBlock))

-- | Return a handler which attempts to parse the specified text as an @AccountAddress@.
--  If the address could not be parsed, an error is logged and a HTTP response with status
--  code @400@ is returned. Takes a string specifying the context from which the function
--  was called, which will be included in the logged message.
doParseAccountAddress :: Text -> Text -> Handler AccountAddress
doParseAccountAddress ctx addrText =
    case addressFromText addrText of
        Left _ -> do
            $(logOther "Trace") $
                "Invalid account address '"
                    <> addrText
                    <> "' for '"
                    <> ctx
                    <> "' request."
            respond400Error EMMalformedAddress RequestInvalid
        Right addr -> return addr

-- | Returns a handler which attempts to get the next account nonce of the specified address.
--  If the address could not be parsed, a HTTP response with status code @400@ is returned.
--  If the account does not exist, a HTTP response with status code @404@ is returned
getAccountNonceR :: Text -> Handler TypedContent
getAccountNonceR addrText = do
    addr <- doParseAccountAddress "getAccountNonce" addrText
    runGRPCWithCustomError
        (Just (StatusNotOkError NOT_FOUND, Nothing, Nothing, Just EMAccountDoesNotExist))
        (getNextSequenceNumber addr)
        $ \nonce -> do
            $(logInfo) "Successfully got nonce."
            sendResponse $ toJSON nonce

-- | Get the account encryption key at the best block.
--  Return @404@ status code if account does not exist in the best block at the moment.
getAccountEncryptionKeyR :: Text -> Handler TypedContent
getAccountEncryptionKeyR addrText = do
    addr <- doParseAccountAddress "getAccountEncryptionKey" addrText
    runGRPCWithCustomError
        (Just (StatusNotOkError NOT_FOUND, Nothing, Nothing, Just EMAccountDoesNotExist))
        (getAccountInfo (AccAddress addr) Best)
        $ \accInfo -> do
            let encryptionKey = aiAccountEncryptionKey accInfo
            $(logInfo) $
                "Retrieved account encryption key for "
                    <> addrText
                    <> ": "
                    <> Text.pack (show encryptionKey)
            sendResponse (object ["accountEncryptionKey" .= encryptionKey])

-- | Get the cost of a transaction, based on its type. The following query parameters are supported
--  - "type", the type of the transaction. This is mandatory.
--  - "numSignatures", the number of signatures on the transaction, defaults to 1 if not present.
--  - "memoSize", the size of the transfer memo. Only supported if the node is running protocol version 2 or higher, and
--    only applies when `type` is either `simpleTransfer` and `encryptedTransfer`.
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
                        Just msize
                            | pv /= P1 -> return $ fromIntegral (2 + msize)
                            | otherwise -> respond404Error EMActionNotCurrentlySupported
        let costResponse energyCost =
                sendResponse $
                    object
                        [ "cost" .= computeCost rate energyCost,
                          "energy" .= energyCost
                        ]
        let costResponseWithIndication (success, energyCost) =
                sendResponse $
                    object
                        [ "cost" .= computeCost rate energyCost,
                          "energy" .= energyCost,
                          "success" .= success
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
                    metasize <-
                        lookupGetParam "metadataSize" >>= \case
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
                    metasize <-
                        lookupGetParam "metadataSize" >>= \case
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
                    invoker <-
                        lookupGetParam "sender" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `sender` value.") RequestInvalid
                            Just val -> case addressFromText val of
                                Left s -> respond400Error (EMParseError $ "Could not parse `sender` value: " ++ s) RequestInvalid
                                Right addr -> return $ Just $ AddressAccount addr

                    contractIndex <-
                        lookupGetParam "contractIndex" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `contractIndex` value.") RequestInvalid
                            Just val -> case readMaybe $ Text.unpack val of
                                Nothing -> respond400Error (EMParseError "Could not parse `contractIndex` value.") RequestInvalid
                                Just index -> return $ ContractIndex index

                    contractSubindex <-
                        lookupGetParam "contractSubindex" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `contractSubindex` value.") RequestInvalid
                            Just val -> case readMaybe $ Text.unpack val of
                                Nothing -> respond400Error (EMParseError "Could not parse `contractSubindex` value.") RequestInvalid
                                Just index -> return $ ContractSubindex index

                    let contract = ContractAddress{..}

                    amount <-
                        lookupGetParam "amount" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `amount` value.") RequestInvalid
                            Just val -> case readMaybe $ Text.unpack val of
                                Nothing -> respond400Error (EMParseError "Could not parse `amount` value.") RequestInvalid
                                Just a -> return a
                    wasmReceiveName <-
                        lookupGetParam "receiveName" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `receiveName` value.") RequestInvalid
                            Just receiveName ->
                                if Wasm.isValidReceiveName receiveName
                                    then return $ Wasm.ReceiveName receiveName
                                    else respond400Error (EMParseError "Invalid receive name.") RequestInvalid
                    wasmParameter <-
                        lookupGetParam "parameter" >>= \case
                            Nothing -> respond400Error (EMParseError "Missing `parameter` value.") RequestInvalid
                            Just parameterText -> case BS16.decode . Text.encodeUtf8 $ parameterText of
                                Left s -> respond400Error (EMParseError $ "Could not parse `parameter` value: " ++ s) RequestInvalid
                                Right parameter -> return $ Wasm.Parameter $ BSS.toShort parameter

                    (energyBufferPercentage :: Int) <-
                        lookupGetParam "executionNRGBuffer" >>= \case
                            Nothing -> return 20
                            Just bufferText -> case readMaybe $ Text.unpack bufferText of
                                Nothing -> respond400Error (EMParseError "Could not parse `executionNRGBuffer` value.") RequestInvalid
                                Just n -> return n

                    let nrg = Energy 500000 -- 500 thousands
                    let pSize =
                            1 -- 1 byte for the payload tag
                                + 8 -- 8 bytes for the amount
                                + 16 -- 16 bytes for the contract address
                                + 2 -- 2 bytes for the length of receive name
                                + Text.length (Wasm.receiveName wasmReceiveName) -- the number of bytes inside the receive name
                                + 2 -- 2 bytes for the length of the parameter
                                + BSS.length (Wasm.parameter wasmParameter) -- the number of bytes inside the parameter
                    let minCost = minimumCost (fromIntegral pSize) numSignatures
                    let invokeContext =
                            InvokeContract.ContractContext
                                { ccInvoker = invoker,
                                  ccContract = contract,
                                  ccAmount = amount,
                                  ccMethod = wasmReceiveName,
                                  ccParameter = wasmParameter,
                                  ccEnergy = nrg
                                }

                    let getInvokeCost = do
                            res <- invokeInstance Best invokeContext
                            -- this is the C_t for the smart contract update transaction
                            return $ case getResponseValueAndHeaders res of
                                Left errRes -> errRes
                                Right (Left err, hds) -> StatusOk $ GRPCResponse hds $ Left err
                                Right (Right execRes, hds) -> StatusOk $ GRPCResponse hds $ Right execRes
                    let cost res = (success, minCost + (invokeCost + (invokeCost * fromIntegral energyBufferPercentage) `div` 100))
                          where
                            (success, invokeCost) = case res of
                                InvokeContract.Success{..} -> (True, rcrUsedEnergy)
                                InvokeContract.Failure{..} -> (False, rcrUsedEnergy)
                    runGRPC getInvokeCost $ costResponseWithIndication . cost
                "updateBakerKeys" -> do
                    let pSize = bakerConfigurePayloadSize False False False True Nothing False False False
                    costResponse $ bakerConfigureEnergyCostWithKeys pSize numSignatures
                "removeBaker" -> do
                    let pSize = bakerConfigurePayloadSize True False False False Nothing False False False
                    costResponse $ bakerConfigureEnergyCostWithoutKeys pSize numSignatures
                "configureBaker" -> do
                    metasize <-
                        lookupGetParam "metadataSize" >>= \case
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
    fetchUpdates :: ClientMonad IO (GRPCResult (Either String (EnergyRate, ProtocolVersion)))
    fetchUpdates = do
        consensusInfoRes <- getConsensusInfo
        case getResponseValueAndHeaders consensusInfoRes of
            Left errRes -> return errRes
            Right (csRes, _) -> do
                chainParamsRes <- getBlockChainParameters Best
                case getResponseValueAndHeaders chainParamsRes of
                    Left errRes -> return errRes
                    Right (cpksRes, hds') -> do
                        let v = do
                                cs <- csRes
                                (EChainParametersAndKeys ecpParams _) <- cpksRes
                                return (_erEnergyRate $ _cpExchangeRates ecpParams, csProtocolVersion cs)
                        return $ StatusOk $ GRPCResponse hds' v
    withExchangeRate = runGRPC fetchUpdates

-- | Returns a @ClientMonad@ which submits the given @BareBlockItem@.
--  Returns @True@ if the @BareBlockItem@ was successfully submitted to the node and @False@ otherwise.
doSendBlockItem :: (MonadIO m) => BareBlockItem -> ClientMonad m (GRPCResult (Either a Bool))
doSendBlockItem item = do
    sbiRes <- sendBlockItem item
    case getResponseValueAndHeaders sbiRes of
        Left _ -> return $ StatusOk $ GRPCResponse [] (Right False)
        Right (_, hds) -> return $ StatusOk $ GRPCResponse hds (Right True)

putCredentialR :: Handler TypedContent
putCredentialR =
    connect rawRequestBody (sinkParserEither json') >>= \case
        Left err -> respond400Error (EMParseError (show err)) RequestInvalid
        Right credJSON ->
            case fromJSON credJSON of
                Error err -> respond400Error (EMParseError err) RequestInvalid
                Success Versioned{..}
                    | vVersion == 0 -> do
                        runGRPC (doSendBlockItem (CredentialDeployment vValue)) $ \case
                            False -> do
                                -- this happens if the request is duplicate, stale, or malformed.
                                $(logError) "Credential rejected by node."
                                respond400Error EMCredentialRejected RequestInvalid
                            True ->
                                sendResponse (object ["submissionId" .= (getHash (CredentialDeployment vValue) :: TransactionHash)])
                    | otherwise -> respond400Error (EMParseError $ "Invalid version number " ++ show vVersion) RequestInvalid

-- | Use the serialize instance of a type to deserialize
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
            case parse transferParser txJSON of
                Error err -> respond400Error (EMParseError err) RequestInvalid
                Success tx -> do
                    $(logInfo) (Text.pack (show tx))
                    runGRPC (doSendBlockItem (NormalTransaction tx)) $ \case
                        False -> do
                            -- transaction invalid
                            $(logError) "Transaction rejected by the node."
                            respond400Error EMTransactionRejected RequestInvalid
                        True ->
                            sendResponse (object ["submissionId" .= (getHash (NormalTransaction tx) :: TransactionHash)])
          where
            transferParser = withObject "Parse transfer request." $ \obj -> do
                sig :: TransactionSignature <- obj .: "signatures"
                body <- decodeBase16 =<< (obj .: "transaction")
                case S.decode (S.encode sig <> body) of
                    Left err -> fail err
                    Right tx -> return tx

-- | An error that is the result of converting a transaction status to a "simple"
--  transaction status.
data OutcomeConversionError
    = -- | Unexpected outcome of a transaction, e.g., transfer without a "transferred" event.
      OCEError !String
    | -- | The transaction type is not supported.
      OCEUnsupportedType

-- | Return a @ClientMonad@ which gets a "simple" status of the transaction with
--  the given @TransactionHash@. If the transaction status is not for one of the
--  supported transactions 'Nothing' is returned.
getSimpleTransactionStatus :: (MonadIO m) => I18n -> TransactionHash -> ClientMonad m (GRPCResult (Either String (Maybe Value)))
getSimpleTransactionStatus i trHash = do
    res <- getBlockItemStatus trHash
    return $ case res of
        StatusOk (GRPCResponse hds val) -> case val of
            Right Received ->
                StatusOk $ GRPCResponse hds $ Right (Just (object ["status" .= String "received"]))
            Right (Committed outcomeMap) -> do
                let outcomes = mapM snd $ Map.toList outcomeMap
                case outcomesToPairs <$> outcomes of
                    Nothing -> StatusOk $ GRPCResponse hds $ Left "Expected exactly one outcome for each committed transaction"
                    Just (Left (OCEError err)) -> StatusOk $ GRPCResponse hds $ Left err
                    Just (Left OCEUnsupportedType) -> StatusOk $ GRPCResponse hds $ Right Nothing
                    Just (Right fields) -> StatusOk $ GRPCResponse hds $ Right (Just (object $ ["status" .= String "committed", "blockHashes" .= (fst <$> Map.toList outcomeMap)] <> fields))
            Right (Finalized bh outcomeM) ->
                case outcomeM of
                    Nothing -> StatusOk $ GRPCResponse hds $ Left "Expected exactly one outcome for a finalized transaction"
                    Just outcome -> do
                        case outcomeToPairs outcome of
                            Left (OCEError err) -> StatusOk $ GRPCResponse hds $ Left err
                            Left OCEUnsupportedType -> StatusOk $ GRPCResponse hds $ Right Nothing
                            Right fields -> StatusOk $ GRPCResponse hds $ Right (Just (object $ ["status" .= String "finalized", "blockHashes" .= [bh :: BlockHash]] <> fields))
            Left err -> StatusOk $ GRPCResponse hds $ Left err
        StatusNotOk (NOT_FOUND, _) -> StatusOk $ GRPCResponse [] $ Right (Just (object ["status" .= String "absent"]))
        StatusNotOk e -> StatusNotOk e
        StatusInvalid -> StatusInvalid
        RequestFailed err -> RequestFailed err
  where
    -- Attach a memo to the pairs.
    addMemo [TransferMemo memo] xs = ("memo" .= memo) : xs
    addMemo _ xs = xs

    outcomeToPairs :: TransactionSummary -> Either OutcomeConversionError [Pair]
    outcomeToPairs TransactionSummary{..} =
        ( [ "transactionHash" .= tsHash,
            "sender" .= tsSender,
            "cost" .= tsCost
          ]
            <>
        )
            <$> case tsType of
                TSTCredentialDeploymentTransaction _ ->
                    -- credential deployment
                    case tsResult of
                        TxSuccess [AccountCreated{}, _] ->
                            return ["outcome" .= String "success"]
                        TxSuccess [CredentialDeployed{}] ->
                            return ["outcome" .= String "success"]
                        es ->
                            Left . OCEError $ "Unexpected outcome of credential deployment: " ++ show es
                (viewTransfer -> True) ->
                    -- transaction is either a transfer or transfer with memo
                    case tsResult of
                        TxSuccess (Transferred{etTo = AddressAccount addr, ..} : mmemo) ->
                            return $
                                addMemo
                                    mmemo
                                    [ "outcome" .= String "success",
                                      "to" .= addr,
                                      "amount" .= etAmount
                                    ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of simple transfer: " ++ show es
                (viewEncryptedTransfer -> True) ->
                    case tsResult of
                        TxSuccess (EncryptedAmountsRemoved{..} : NewEncryptedAmount{..} : mmemo) ->
                            return $
                                addMemo
                                    mmemo
                                    [ "outcome" .= String "success",
                                      "sender" .= earAccount,
                                      "to" .= neaAccount,
                                      "encryptedAmount" .= neaEncryptedAmount,
                                      "inputEncryptedAmount" .= earInputAmount,
                                      "aggregatedIndex" .= earUpToIndex,
                                      "newSelfEncryptedAmount" .= earNewAmount
                                    ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of encrypted transfer: " ++ show es
                TSTAccountTransaction (Just TTTransferToPublic) ->
                    case tsResult of
                        TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}] ->
                            return
                                [ "outcome" .= String "success",
                                  "sender" .= earAccount,
                                  "newSelfEncryptedAmount" .= earNewAmount,
                                  "inputEncryptedAmount" .= earInputAmount,
                                  "aggregatedIndex" .= earUpToIndex,
                                  "amountAdded" .= aabdAmount
                                ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of secret to public transfer: " ++ show es
                TSTAccountTransaction (Just TTTransferToEncrypted) ->
                    case tsResult of
                        TxSuccess [EncryptedSelfAmountAdded{..}] ->
                            return
                                [ "outcome" .= String "success",
                                  "sender" .= eaaAccount,
                                  "newSelfEncryptedAmount" .= eaaNewAmount,
                                  "amountSubtracted" .= eaaAmount
                                ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of public to secret transfer: " ++ show es
                TSTAccountTransaction (Just TTConfigureBaker) ->
                    case tsResult of
                        TxSuccess ((eventBakerId -> (Just bid)) : _) ->
                            return
                                [ "outcome" .= String "success",
                                  "bakerId" .= bid
                                ]
                        TxSuccess _ -> return ["outcome" .= String "success"]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                TSTAccountTransaction (Just TTConfigureDelegation) ->
                    case tsResult of
                        TxSuccess _ -> return ["outcome" .= String "success"]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                TSTAccountTransaction (Just TTDeployModule) ->
                    case tsResult of
                        TxSuccess [ModuleDeployed mref] ->
                            return
                                [ "outcome" .= String "success",
                                  "moduleRef" .= mref
                                ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of deploying module: " ++ show es
                TSTAccountTransaction (Just TTInitContract) ->
                    case tsResult of
                        TxSuccess [ContractInitialized{..}] ->
                            return
                                [ "outcome" .= String "success",
                                  "moduleRef" .= ecRef,
                                  "address" .= ecAddress,
                                  "amount" .= ecAmount,
                                  "initName" .= ecInitName,
                                  "contractVersion" .= ecContractVersion,
                                  "events" .= ecEvents
                                ]
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                        es ->
                            Left . OCEError $ "Unexpected outcome of initialized module: " ++ show es
                TSTAccountTransaction (Just TTUpdate) ->
                    case tsResult of
                        TxSuccess events ->
                            case eventsToMaybeValues events of
                                Just vals ->
                                    return
                                        [ "outcome" .= String "success",
                                          "trace" .= vals
                                        ]
                                Nothing -> Left . OCEError $ "Unexpected outcome of updating module: " ++ show tsResult
                        TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                _ -> case tsResult of
                    TxReject reason -> return ["outcome" .= String "reject", "rejectReason" .= i18n i reason]
                    _ -> Left OCEUnsupportedType
    outcomesToPairs :: [TransactionSummary] -> Either OutcomeConversionError [Pair]
    outcomesToPairs l = do
        outcomes <- mapM outcomeToPairs l
        case outcomes of
            [] -> Left . OCEError $ "Expected at least one transaction outcome for a committed transaction"
            [o] -> return o
            (h : r)
                | all (h ==) r -> return h
                | otherwise -> return ["outcome" .= String "ambiguous"]
    -- This function returns the JSON representing an event that can occur due to a smart contract update transaction.
    -- It returns @Nothing@ if used on an event different from the four smart contract update events `Updated`, `Transferred`,
    -- `Interrupted`, `Resumed`.
    updateEventToMaybeValue :: Event -> Maybe Value
    updateEventToMaybeValue Updated{..} =
        Just $
            object
                [ "type" .= String "updated",
                  "address" .= euAddress,
                  "instigator" .= euInstigator,
                  "amount" .= euAmount,
                  "message" .= euMessage,
                  "receiveName" .= euReceiveName,
                  "contractVersion" .= euContractVersion,
                  "events" .= euEvents
                ]
    updateEventToMaybeValue Transferred{etFrom = AddressContract addrFrom, etTo = AddressAccount addrTo, ..} =
        Just $
            object
                [ "type" .= String "transferred",
                  "from" .= addrFrom,
                  "amount" .= etAmount,
                  "to" .= addrTo
                ]
    updateEventToMaybeValue Interrupted{..} =
        Just $
            object
                [ "type" .= String "interrupted",
                  "address" .= iAddress,
                  "events" .= iEvents
                ]
    updateEventToMaybeValue Resumed{..} =
        Just $
            object
                [ "type" .= String "resumed",
                  "address" .= rAddress,
                  "success" .= rSuccess
                ]
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

-- | Get the baker ID from a baker configuration event
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
            runGRPC (getSimpleTransactionStatus i txHash) $ \case
                Nothing -> respond400Error (EMGRPCErrorResponse "Unsupported transaction type for simple statuses.") RequestInvalid
                Just v -> sendResponse v

-- | Whether to include memos in formatted account transactions or not.
--  If not, transfers with memos are mapped to a corresponding transfer without a memo.
data IncludeMemos = IncludeMemo | ExcludeMemo
    deriving (Eq, Show)

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
    let makeJsonEntries =
            map
                ( \(Entity key CIS2Entry{..}) ->
                    object
                        [ "token" .= cIS2EntryToken_id,
                          -- amounts are always integers, so we just take the numerator
                          -- since the integers can be very large we make them into a string
                          "totalSupply" .= show (numerator cIS2EntryTotal_supply),
                          "id" .= key
                        ]
                )
    sendResponse $
        object $
            [ "limit" .= limit,
              "count" .= length entries,
              "tokens" .= makeJsonEntries entries
            ]
                <> maybeToList (("from" .=) <$> mfrom)

-- | Lookup token ids from the tokenId query parameter, and attempt to parse
--  them as a comma-separated list. Responds with an invalid request error
--  in case parsing is unsuccessful.
parseCIS2TokenIds :: Handler [TokenId]
parseCIS2TokenIds = do
    param <-
        lookupGetParam "tokenId" >>= \case
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
                    object
                        [ "contractName" .= name,
                          "metadata"
                            .= AE.toJSON
                                ( zipWith
                                    ( \tid md ->
                                        object
                                            [ "tokenId" .= tid,
                                              "metadataURL" .= muURL md,
                                              "metadataChecksum" .= muChecksum md
                                            ]
                                    )
                                    tids
                                    urls
                                )
                        ]

getCIS2TokenMetadataV1 :: Word64 -> Word64 -> Handler TypedContent
getCIS2TokenMetadataV1 index subindex = do
    let nrg = Energy 50_000 -- ~50ms worth of
    let contractAddr = ContractAddress (ContractIndex index) (ContractSubindex subindex)
    tids <- parseCIS2TokenIds
    unless (length tids <= 20) $ respond400Error (EMParseError "Too many tokens requested.") RequestInvalid
    let getURLs = do
            len <- S.getWord16le
            unless (len == 1) $ fail "Expected one URL."
            replicateM (fromIntegral len) getMetadataUrl
    (lf, iInfo) <- retrieveInstanceInfo contractAddr
    responses <- forM tids $ \tid -> do
        let serializedParam = Wasm.Parameter . BSS.toShort . S.runPut $ do
                S.putWord16le 1
                S.put tid
        cis2InvokeHelperError lf contractAddr iInfo (Wasm.EntrypointName "tokenMetadata") serializedParam nrg $
            \case
                (_, InvokeContract.Success{..})
                    | Just rv <- rcrReturnValue,
                      Right [md] <- S.runGet getURLs rv -> do
                        return . Just $
                            object
                                [ "tokenId" .= tid,
                                  "metadataURL" .= muURL md,
                                  "metadataChecksum" .= muChecksum md
                                ]
                (_, _notSuccess) -> return Nothing
    sendResponse $
        object
            [ "contractName" .= (Wasm.initContractName (Wasm.iiName iInfo)),
              "metadata" .= AE.toJSON (catMaybes responses)
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
                        unless (fromIntegral len == length tids) $ fail "Unexpected response length."
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
                                            [ "tokenId" .= tid,
                                              "balance" .= bal
                                            ]
                                    )
                                    tids
                                    urls
                                )

-- | Like 'getCIS2TokenBalance' but instead of making one query to the node
--  it checks each token individually and responds with a partial list of responses.
getCIS2TokenBalanceV1 :: Word64 -> Word64 -> Text -> Handler TypedContent
getCIS2TokenBalanceV1 index subindex addrText = do
    let nrg = Energy 50_000 -- ~50ms worth of
    let contractAddr = ContractAddress (ContractIndex index) (ContractSubindex subindex)
    let getBalances = do
            len <- S.getWord16le
            unless (len == 1) $ fail "Unexpected response."
            replicateM (fromIntegral len) getTokenBalance
    case addressFromText addrText of
        Left err -> respond400Error (EMParseError err) RequestInvalid
        Right addr -> do
            tids <- parseCIS2TokenIds
            unless (length tids <= 20) $ respond400Error (EMParseError "Too many tokens requested.") RequestInvalid
            (lf, iInfo) <- retrieveInstanceInfo contractAddr
            json <- forM tids $ \tid -> do
                let serializedParam = Wasm.Parameter . BSS.toShort . S.runPut $ do
                        S.putWord16le 1
                        S.put tid
                        S.put (AddressAccount addr)
                cis2InvokeHelperError lf contractAddr iInfo (Wasm.EntrypointName "balanceOf") serializedParam nrg $
                    \case
                        (_, InvokeContract.Success{..})
                            | Just rv <- rcrReturnValue,
                              Right [bal] <- S.runGet getBalances rv -> do
                                return $
                                    Just $
                                        object
                                            [ "tokenId" .= tid,
                                              "balance" .= bal
                                            ]
                        _notSuccess -> do
                            return $ Nothing
            sendResponse $ AE.toJSON (catMaybes json)

-- | Helper to handle the boilerplate common to both the metadata and balance of
--  queries. It handles getting the address of a contract, handling errors in
--  invocations, and calling the respective handlers for the specific query via
--  the continuation.
cis2InvokeHelperError ::
    -- | Block hash to query in.
    BlockHash ->
    -- | Address of the instance.
    ContractAddress ->
    -- | Instance information.
    Wasm.InstanceInfo ->
    -- | Its entrypoint.
    Wasm.EntrypointName ->
    -- | The parameter to invoke
    Wasm.Parameter ->
    -- | Energy to allow for the invoke.
    Energy ->
    -- | Continuation applied to the name of the contract (without @_init@) and the response of the query.
    ((Text, InvokeContract.InvokeContractResult) -> Handler c) ->
    HandlerFor Proxy c
cis2InvokeHelperError lf contractAddr iInfo entrypoint serializedParam nrg k = do
    let contractName = Wasm.iiName iInfo
    let invokeContext =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccContract = contractAddr,
                  ccAmount = 0,
                  ccMethod = Wasm.uncheckedMakeReceiveName contractName entrypoint,
                  ccParameter = serializedParam,
                  ccEnergy = nrg
                }
    let query = do
            iInstanceRes <- invokeInstance (Given lf) invokeContext
            case getResponseValueAndHeaders iInstanceRes of
                Left errRes -> return errRes
                Right (v, hds') -> return $ StatusOk $ GRPCResponse hds' $ fmap (Wasm.initContractName contractName,) v
    runGRPCWithCustomError
        (Just (StatusNotOkError NOT_FOUND, Nothing, Nothing, Just $ EMErrorResponse NotFound))
        query
        k

-- | Get the instance information, failing if the instance does not exist.
-- Returns the block which was queried and the instance information.
retrieveInstanceInfo ::
    -- | Address of the contract to query
    ContractAddress ->
    HandlerFor Proxy (BlockHash, Wasm.InstanceInfo)
retrieveInstanceInfo contractAddr = do
    let query = do
            iInfoRes <- getInstanceInfo contractAddr LastFinal
            case getResponseValueAndHeaders iInfoRes of
                Left errRes -> return errRes
                Right (Left err, hds) -> return $ StatusOk $ GRPCResponse hds $ Left err
                Right (Right iInfo, hds) -> do
                    lf <- getBlockHashHeader hds
                    return $ StatusOk $ GRPCResponse hds $ Right (lf, iInfo)
    runGRPCWithCustomError
        (Just (StatusNotOkError NOT_FOUND, Nothing, Nothing, Just $ EMErrorResponse NotFound))
        query
        return

-- | Helper to handle the boilerplate common to both the metadata and balance of
--  queries. It handles getting the address of a contract, handling errors in
--  invocations, and calling the respective handlers for the specific query via
--  the continuation. This helper responds with a 400 invalid request error if
--  the contract query cannot be successfully completed.
cis2InvokeHelper ::
    -- | Address of the contract to invoke.
    ContractAddress ->
    -- | Its entrypoint.
    Wasm.EntrypointName ->
    -- | The parameter to invoke
    Wasm.Parameter ->
    -- | Energy to allow for the invoke.
    Energy ->
    -- | Continuation applied to the name of the contract (without @_init@) and the return value produced by a successful result.
    (Text -> BS8.ByteString -> Handler c) ->
    HandlerFor Proxy c
cis2InvokeHelper contractAddr entrypoint serializedParam nrg k = do
    (bh, iInfo) <- retrieveInstanceInfo contractAddr
    cis2InvokeHelperError bh contractAddr iInfo entrypoint serializedParam nrg $
        \case
            (_, InvokeContract.Failure{..}) -> do
                $logDebug $ "Invoke failed: " <> Text.pack (show rcrReason)
                respond400Error EMInvokeFailed RequestInvalid
            (name, InvokeContract.Success{..}) -> do
                case rcrReturnValue of
                    Nothing -> respond400Error EMV0Contract RequestInvalid
                    Just rv -> k name rv

-- | Balance of a CIS2 token.
newtype TokenBalance = TokenBalance Integer
    deriving (Show)

instance AE.ToJSON TokenBalance where
    toJSON (TokenBalance i) = toJSON (show i)

-- | A custom parser for a CIS2 token that uses LEB128 to parse the token
--  balance.
getTokenBalance :: S.Get TokenBalance
getTokenBalance = TokenBalance <$> go 0 0
  where
    go !acc !s
        | s >= 37 = fail "Invalid token amount encoding."
        | otherwise = do
            n <- S.getWord8
            if testBit n 7
                then go (acc + (toInteger (clearBit n 7) `shiftL` (s * 7))) (s + 1)
                else return $! (acc + toInteger n `shiftL` (s * 7))

newtype Checksum = Checksum Hash
    deriving (Show, AE.ToJSON, AE.FromJSON, S.Serialize)

-- | CIS2 token metadata URL.
data MetadataURL = MetadataURL
    { -- | Metadata URL.
      muURL :: !Text,
      -- | Optional checksum (sha256) of the contents of the metadata URL.
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

-- | List transactions for the account.
getAccountTransactionsWorker :: IncludeMemos -> Text -> Handler TypedContent
getAccountTransactionsWorker includeMemos addrText = do
    i <- internationalize
    -- Get the canonical address of the account.
    let getAddress k = do
            givenAddr <- doParseAccountAddress "getAccountTransactionsWorker" addrText
            let doGetAccAddress = do
                    ai <- getAccountInfo (AccAddress givenAddr) LastFinal
                    return $ case ai of
                        StatusOk (GRPCResponse hds (Right accInfo)) ->
                            StatusOk $ GRPCResponse hds $ Right (aiAccountAddress accInfo)
                        StatusOk (GRPCResponse hds (Left err)) ->
                            StatusOk $ GRPCResponse hds $ Left err
                        StatusNotOk (NOT_FOUND, _) ->
                            -- If the account does not exist on the node, assume the given address is the one we want.
                            StatusOk $ GRPCResponse [] $ Right givenAddr
                        StatusInvalid -> StatusInvalid
                        StatusNotOk err -> StatusNotOk err
                        RequestFailed err -> RequestFailed err
            runGRPC doGetAccAddress k
    getAddress $ \addr -> do
        order <- lookupGetParam "order"
        let (ordering, ordType :: Text, ordRel) = case order of
                Just (Text.unpack . Text.toLower -> ('d' : _)) -> (E.desc, "descending", (E.<.))
                _ -> (E.asc, "ascending", (E.>.))
        startId :: Maybe EntryId <- (>>= fromPathPiece) <$> lookupGetParam "from"
        limit <- maybe 20 (max 0 . min 1000) . (>>= readMaybe . Text.unpack) <$> lookupGetParam "limit"

        -- Exclude any transactions with block time earlier than `blockTimeFrom` (seconds after epoch)
        maybeTimeFromFilter <-
            lookupGetParam "blockTimeFrom" <&> \case
                Nothing -> Just $ const $ return () -- the default: exclude nothing.
                Just fromTime ->
                    case readMaybe $ Text.unpack fromTime of
                        Nothing -> Nothing
                        Just seconds -> Just $ \s ->
                            E.where_ (s E.^. SummaryTimestamp E.>=. (E.val Timestamp{tsMillis = seconds * 1000}))

        -- Exclude any transactions with block time later than `blockTimeTo` (seconds after epoch)
        maybeTimeToFilter <-
            lookupGetParam "blockTimeTo" <&> \case
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
        let isAccountTransaction = \s -> coerced s EJ.?. "Left" -- Left are account transactions.
        let extractedTag = \s -> coerced s EJ.#>>. ["Right", "tag"] -- the reward tag.
        maybeTypeFilter <-
            lookupGetParam "includeRewards" <&> \case
                Nothing -> Just $ const $ return () -- the default
                Just "all" -> Just $ const $ return ()
                -- check if
                -- - either the transaction is an account transaction
                -- - or if not check that it is not a finalization reward.
                Just "allButFinalization" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "FinalizationRewards"))
                Just "none" -> Just $ \s -> E.where_ $ isAccountTransaction s
                Just _ -> Nothing

        maybeBlockRewardFilter <-
            lookupGetParam "blockRewards" <&> \case
                Nothing -> Just $ const $ return () -- the default: do not exclude block rewards.
                Just "y" -> Just $ const $ return ()
                -- check if
                -- - either the transaction is an account transaction
                -- - or if not check that it is not a block reward.
                Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "BlockReward"))
                Just _ -> Nothing

        maybeFinalizationRewardFilter <-
            lookupGetParam "finalizationRewards" <&> \case
                Nothing -> Just $ const $ return () -- the default: do not exclude finalization rewards.
                Just "y" -> Just $ const $ return ()
                -- check if
                -- - either the transaction is an account transaction
                -- - or if not check that it is not a finalization reward.
                Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "FinalizationRewards"))
                Just _ -> Nothing

        maybeBakingRewardFilter <-
            lookupGetParam "bakingRewards" <&> \case
                Nothing -> Just $ const $ return () -- the default: do not exclude baking rewards.
                Just "y" -> Just $ const $ return ()
                -- check if
                -- - either the transaction is an account transaction
                -- - or if not check that it is not a baking reward.
                Just "n" -> Just $ \s -> E.where_ (isAccountTransaction s E.||. extractedTag s E.!=. E.val (Just "BakingRewards"))
                Just _ -> Nothing

        maybeEncryptedFilter <-
            lookupGetParam "onlyEncrypted" <&> \case
                Nothing -> Just $ const $ return () -- the default: include all transactions.
                Just "n" -> Just $ const $ return ()
                Just "y" -> Just $ \s ->
                    let extractedType = coerced s EJ.#>>. ["Left", "type", "contents"] -- the transaction type.
                    -- check if the transaction is encrypting, decrypting, or transferring an encrypted amount.
                    in  E.where_ $
                            extractedType E.==. E.val (Just "encryptedAmountTransfer")
                                E.||. extractedType E.==. E.val (Just "transferToEncrypted")
                                E.||. extractedType E.==. E.val (Just "transferToPublic")
                                E.||. extractedType E.==. E.val (Just "encryptedAmountTransferWithMemo")
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
                    E.select $ E.from $ \(e `E.InnerJoin` s) -> do
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
                        sendResponseStatus badGateway502 $
                            object
                                [ "errorMessage" .= i18n i EMDatabaseError,
                                  "error" .= fromEnum InternalError
                                ]
                    Right fentries ->
                        sendResponse $
                            object $
                                [ "limit" .= limit,
                                  "order" .= ordType,
                                  "count" .= length fentries,
                                  "transactions" .= fentries
                                ]
                                    <> (maybe [] (\sid -> ["from" .= sid]) startId)

-- | Convert a timestamp to seconds since the unix epoch. A timestamp can be a fractional number, e.g., 17.5.
timestampToFracSeconds :: Timestamp -> Double
timestampToFracSeconds Timestamp{..} = fromRational (toInteger tsMillis Rational.% 1000)

-- | Format a transaction affecting an account.
formatEntry ::
    -- | Whether to include memos in the enties. If not,
    --  then transfers with memos are mapped to
    --  corresponding transfers without memos.
    IncludeMemos ->
    -- | Whether to include a raw reject reason for account transactions or not.
    Bool ->
    -- | Internationalization of messages.
    I18n ->
    -- | Address of the account whose transactions we are formatting.
    AccountAddress ->
    -- | Database entry to be formatted.
    (Entity Entry, Entity Summary) ->
    Either String Value
formatEntry includeMemos rawRejectReason i self (Entity key Entry{}, Entity _ Summary{..}) = do
    let blockDetails =
            [ "blockHash" .= unBSS summaryBlock,
              "blockTime" .= timestampToFracSeconds summaryTimestamp
            ]
    transactionDetails <- case AE.fromJSON summarySummary of
        AE.Error e -> Left e
        AE.Success (Right v@BakingRewards{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  -- This lookup is correct in presence of aliases since rewards are given to canonical addresses.
                  "total" .= Map.lookup self (accountAmounts stoBakerRewards), -- this should always return Just due to the way we look up.
                  "details"
                    .= object
                        [ "type" .= ("bakingReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@Mint{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total" .= stoMintPlatformDevelopmentCharge, -- this will only happen if we are looking up the foundation account
                  "details"
                    .= object
                        [ "type" .= ("platformDevelopmentCharge" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@FinalizationRewards{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  -- This lookup is correct in presence of aliases since rewards are given to canonical addresses.
                  "total" .= Map.lookup self (accountAmounts stoFinalizationRewards), -- this should always return Just due to the way we look up.
                  "details"
                    .= object
                        [ "type" .= ("finalizationReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@BlockReward{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total"
                    .= if sameAccount self stoBaker && sameAccount self stoFoundationAccount
                        then stoBakerReward + stoFoundationCharge
                        else
                            if sameAccount self stoBaker
                                then stoBakerReward
                                else stoFoundationCharge, -- due to the way we index, that is the only remaining option
                  "details"
                    .= object
                        [ "type" .= ("blockReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@PaydayFoundationReward{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total" .= stoDevelopmentCharge,
                  "details"
                    .= object
                        [ "type" .= ("paydayFoundationReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@PaydayAccountReward{..}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total" .= (stoTransactionFees + stoBakerReward + stoFinalizationReward),
                  "details"
                    .= object
                        [ "type" .= ("paydayAccountReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@BlockAccrueReward{}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total" .= (0 :: Amount), -- Zero, since this is not a payment to a specific account
                  "details"
                    .= object
                        [ "type" .= ("blockAccrueReward" :: Text),
                          "outcome" .= ("success" :: Text),
                          "description" .= i18n i (ShortDescription v),
                          "events" .= [i18n i v]
                        ]
                ]
        AE.Success (Right v@PaydayPoolReward{}) ->
            return
                [ "origin" .= object ["type" .= ("reward" :: Text)],
                  "total" .= (0 :: Amount), -- Zero, since this is not a payment to a specific account
                  "details"
                    .= object
                        [ "type" .= ("paydayPoolReward" :: Text),
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
                            [TransferMemo{..}] -> ("memo" .= tmMemo) : ps
                            _ -> ps
            let (origin, selfOrigin) = case tsSender of
                    Just sender
                        | sameAccount sender self -> (object ["type" .= ("self" :: Text)], True)
                        | otherwise -> (object ["type" .= ("account" :: Text), "address" .= sender], False)
                    Nothing -> (object ["type" .= ("none" :: Text)], False)

                -- If ExcludeMemo then we filter out the TransferMemo event from the
                -- list of events in order to maintain backwards compatibility.
                filterTransferMemo x =
                    includeMemos == IncludeMemo || case x of
                        TransferMemo{} -> False
                        _ -> True
                (resultDetails, subtotal) = case tsResult of
                    TxSuccess evts ->
                        ( ( ["outcome" .= ("success" :: Text), "events" .= (fmap (i18n i) . filter filterTransferMemo $ evts)]
                                <> case (tsType, evts) of
                                    (viewTransfer -> True, Transferred (AddressAccount fromAddr) amt (AddressAccount toAddr) : mmemo) ->
                                        addMemo
                                            mmemo
                                            [ "transferSource" .= fromAddr,
                                              "transferDestination" .= toAddr,
                                              "transferAmount" .= amt
                                            ]
                                    (viewEncryptedTransfer -> True, EncryptedAmountsRemoved{..} : NewEncryptedAmount{..} : mmemo) ->
                                        addMemo
                                            mmemo
                                            [ "transferSource" .= earAccount,
                                              "transferDestination" .= neaAccount,
                                              "encryptedAmount" .= neaEncryptedAmount,
                                              "aggregatedIndex" .= earUpToIndex,
                                              "inputEncryptedAmount" .= earInputAmount,
                                              "newIndex" .= neaNewIndex,
                                              "newSelfEncryptedAmount" .= earNewAmount
                                            ]
                                    (TSTAccountTransaction (Just TTTransferToPublic), [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}]) ->
                                        [ "transferSource" .= earAccount,
                                          "amountAdded" .= aabdAmount,
                                          "aggregatedIndex" .= earUpToIndex,
                                          "inputEncryptedAmount" .= earInputAmount,
                                          "newSelfEncryptedAmount" .= earNewAmount
                                        ]
                                    (TSTAccountTransaction (Just TTTransferToEncrypted), [EncryptedSelfAmountAdded{..}]) ->
                                        [ "transferSource" .= eaaAccount,
                                          "amountSubtracted" .= eaaAmount,
                                          "newSelfEncryptedAmount" .= eaaNewAmount
                                        ]
                                    (viewScheduledTransfer -> True, TransferredWithSchedule{..} : mmemo) ->
                                        addMemo
                                            mmemo
                                            [ "transferDestination" .= etwsTo,
                                              "transferAmount" .= foldl' (+) 0 (map snd etwsAmount)
                                            ]
                                    (TSTAccountTransaction (Just TTRegisterData), [DataRegistered{..}]) ->
                                        ["registeredData" .= drData]
                                    _ -> []
                          ),
                          eventSubtotal self evts
                        )
                    TxReject reason ->
                        let rawReason = if rawRejectReason then ["rawRejectReason" .= reason] else []
                        in  (["outcome" .= ("reject" :: Text), "rejectReason" .= i18n i reason] ++ rawReason, Nothing)

                details = case includeMemos of
                    IncludeMemo -> object $ ["type" .= renderTransactionSummaryType tsType, "description" .= i18n i tsType] <> resultDetails
                    ExcludeMemo -> object $ ["type" .= renderTransactionSummaryType (forgetMemoInSummary tsType), "description" .= i18n i tsType] <> resultDetails

                costs
                    | selfOrigin = case subtotal of
                        Nothing -> let total = -toInteger tsCost in ["cost" .= show (toInteger tsCost), "total" .= show total]
                        Just st ->
                            let total = st - toInteger tsCost
                            in  ["subtotal" .= show st, "cost" .= show (toInteger tsCost), "total" .= show total]
                    | otherwise = ["total" .= show (fromMaybe 0 subtotal)]

                encryptedCost = case tsSender of
                    Just sender
                        | sameAccount sender self -> case (tsType, tsResult) of
                            (viewEncryptedTransfer -> True, TxSuccess (EncryptedAmountsRemoved{..} : NewEncryptedAmount{..} : _)) ->
                                [ "encrypted"
                                    .= object
                                        [ "encryptedAmount" .= neaEncryptedAmount,
                                          "newStartIndex" .= earUpToIndex,
                                          "newSelfEncryptedAmount" .= earNewAmount
                                        ]
                                ]
                            (TSTAccountTransaction (Just TTTransferToPublic), TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{}]) ->
                                [ "encrypted"
                                    .= object
                                        [ "newStartIndex" .= earUpToIndex,
                                          "newSelfEncryptedAmount" .= earNewAmount
                                        ]
                                ]
                            (TSTAccountTransaction (Just TTTransferToEncrypted), TxSuccess [EncryptedSelfAmountAdded{..}]) ->
                                ["encrypted" .= object ["newSelfEncryptedAmount" .= eaaNewAmount]]
                            _ -> []
                        | otherwise -> case (tsType, tsResult) of
                            (viewEncryptedTransfer -> True, TxSuccess (EncryptedAmountsRemoved{} : NewEncryptedAmount{..} : _)) ->
                                [ "encrypted"
                                    .= object
                                        [ "encryptedAmount" .= neaEncryptedAmount,
                                          "newIndex" .= neaNewIndex
                                        ]
                                ]
                            _ -> []
                    Nothing -> []
            return $
                [ "origin" .= origin,
                  "energy" .= tsEnergyCost,
                  "details" .= details,
                  "transactionHash" .= tsHash
                ]
                    <> costs
                    <> encryptedCost
    return $ object $ ["id" .= key] <> blockDetails <> transactionDetails

-- | Check whether the two addresses point to the same account. In protocol
--  versions 1 and 2 this should just be account address equality, and in
--  protocol version 3 it should technically be checking on the chain as well
--  since in principle there might be accounts which clash on the first 29 bytes.
--  But that will not happen in practice and if it does before we update to P3 we
--  can update the proxy with a more expensive and complex check. After we have
--  successfully updated to P3 and there have been no clashes, there will be no
--  further possible clashes.
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
    eventCost ContractInitialized{..} = Just (-toInteger ecAmount)
    eventCost Updated{..}
        | isSelf euInstigator = Just (-toInteger euAmount)
        | otherwise = Nothing
    eventCost Transferred{..} = case (isSelf etFrom, isSelf etTo) of
        (True, True) -> Just 0
        (True, False) -> Just (-toInteger etAmount)
        (False, True) -> Just (toInteger etAmount)
        (False, False) -> Nothing
    eventCost TransferredWithSchedule{..} =
        if sameAccount self etwsFrom -- self transfers are not possible with schedule
            then Just (-toInteger (sum . map snd $ etwsAmount))
            else Just (toInteger (sum . map snd $ etwsAmount))
    eventCost AmountAddedByDecryption{..} = Just $ toInteger aabdAmount
    eventCost EncryptedSelfAmountAdded{..} = Just $ -toInteger eaaAmount
    eventCost _ = Nothing

-- | Try to execute a GTU drop to the given account.
--
-- 1.  Lookup the account on the chain
--        - If it's not finalized, return 404 Not Found (account not final)
-- 2.  Lookup the account in the database
--    A. If there is an entry,
--        A.1. query the send account's last finalized nonce and balance
--          - on failure, return 502 Bad Gateway (configuration error)
--        A.2. determine the state of the transaction
--        - received, committed, or successfully finalized: report transaction hash
--        - absent, or unsuccessfully finalized:
--          - if the GTU drop account has insufficient funds, return 502 Bad Gateway (configuration error)
--          - if the transaction nonce is finalized or the transaction is expired, delete the entry and retry from 2
--          - otherwise, send the transaction to the node and report the transaction hash
--    B. If there is no entry
--        B.1. query the sender's next available nonce
--        B.2. produce/sign the transaction
--        B.3. store the transaction in the database
--            - on failure, retry from 2
--        B.4. submit the transaction and report transaction hash
data DropSchedule = Scheduled | Normal

instance FromJSON DropSchedule where
    parseJSON = withObject "Drop schedule" $ \obj -> do
        ty <- obj .: "type"
        if ty == Text.pack "scheduled"
            then return Scheduled
            else
                if ty == Text.pack "normal"
                    then return Normal
                    else fail "Unsupported drop type."

-- | Return a handler which performs a GTU drop to the specified account.
--  Attempt to make a GTU drop to the account with the specified address if @gtuDropData@ was provided
--  in the environment settings. Responds with a 404-status code if @gtuDropData@ was absent.
putGTUDropR :: Text -> Handler TypedContent
putGTUDropR addrText = do
    Proxy{..} <- getYesod
    case gtuDropData of
        Nothing -> respond404Error $ EMErrorResponse NotFound
        Just gtuDropData' -> do
            addr <- doParseAccountAddress "putGTUDrop" addrText
            -- Check that the account exists in a finalized block.
            runGRPCWithCustomError
                (Just (StatusNotOkError NOT_FOUND, Just badRequest400, Just RequestInvalid, Just $ EMAccountNotFinal))
                (getAccountInfo (AccAddress addr) LastFinal)
                $ \_ -> do
                    connect rawRequestBody (sinkParserEither json') >>= \case
                        Left _ -> tryDrop addr Normal gtuDropData' -- malformed or empty body, we assume normal drop.
                        Right v -> case fromJSON v of
                            AE.Success x -> tryDrop addr x gtuDropData'
                            AE.Error e -> do
                                $(logWarn) (Text.pack e)
                                respond400Error EMConfigurationError RequestInvalid
  where
    doGetAccInfo :: AccountAddress -> ClientMonad IO (GRPCResult (Either String (Nonce, Amount)))
    doGetAccInfo t = do
        aiRes <- getAccountInfo (AccAddress t) LastFinal
        case getResponseValueAndHeaders aiRes of
            Left errRes -> return errRes
            Right (val, hds) -> do
                let v = do
                        ai <- val
                        return (aiAccountNonce ai, aiAccountAmount ai)
                return $ StatusOk $ GRPCResponse hds v
    -- Determine if the transaction is or could become
    -- successfully finalized. Returns @False@ if the
    -- transaction is absent or is finalized but failed.
    doIsTransactionOK trHash = do
        bisRes <- getBlockItemStatus trHash
        return $ case bisRes of
            -- Transaction was found
            StatusOk (GRPCResponse hds val) -> case val of
                Right Received ->
                    StatusOk $ GRPCResponse hds $ Right True
                Right (Committed _) ->
                    StatusOk $ GRPCResponse hds $ Right True
                Right (Finalized _ outcomeM) ->
                    case outcomeM of
                        Nothing -> StatusOk $ GRPCResponse hds $ Left "Expected exactly one outcome for a finalized transaction"
                        Just TransactionSummary{..} ->
                            case tsResult of
                                TxSuccess{} -> StatusOk $ GRPCResponse hds $ Right True
                                -- Transaction was finalized but it was rejected.
                                TxReject{} -> StatusOk $ GRPCResponse hds $ Right False
                Left err -> StatusOk $ GRPCResponse hds $ Left err
            -- Transaction is absent
            StatusNotOk (NOT_FOUND, _) -> StatusOk $ GRPCResponse [] $ Right False
            -- Some other error occurred.
            StatusNotOk e -> StatusNotOk e
            StatusInvalid -> StatusInvalid
            RequestFailed err -> RequestFailed err
    sendTransaction transaction =
        runGRPC (doSendBlockItem $ NormalTransaction transaction) $ \case
            False -> do
                -- this case cannot happen at this time
                $(logError) "GTU drop transaction rejected by node."
                respond400Error EMConfigurationError RequestInvalid
            True ->
                sendResponse (object ["submissionId" .= (getHash (NormalTransaction transaction) :: TransactionHash)])
    configErr = do
        i <- internationalize
        sendResponseStatus badGateway502 $
            object
                [ "errorMessage" .= i18n i EMConfigurationError,
                  "error" .= fromEnum InternalError
                ]
    tryDrop addr dropType gtuDropData@GTUDropData{..} = do
        -- number of keys we need to sign with
        let numKeys = sum . map (length . snd) $ dropKeys
        -- Determine if there is already a GTU drop entry for this account
        rcpRecord <- runDB $ getBy (UniqueAccount (ByteStringSerialized addr))
        case rcpRecord of
            -- If there is no entry, try the drop
            Nothing -> runGRPC (getNextSequenceNumber dropAccount) $ \nonce -> do
                currentTime <- liftIO $ round <$> getPOSIXTime
                (payload, thEnergyAmount) <- case dropType of
                    Normal -> return (Transfer addr dropAmount, simpleTransferEnergyCost simpleTransferPayloadSize numKeys)
                    Scheduled -> do
                        -- sample a random release schedule spaced by 5min.
                        numRels <- liftIO $ randomRIO (1 :: Word, 15)
                        let start = Timestamp{tsMillis = currentTime * 1000 + 300 * 1000}
                            (releaseAmount, remainder) = (fromIntegral dropAmount :: Word) `divMod` fromIntegral numRels
                            releases =
                                [ ( addDuration start (fromIntegral i * 300 * 1000),
                                    if i == 1
                                        then fromIntegral (remainder + releaseAmount)
                                        else fromIntegral releaseAmount
                                  )
                                  | i <- [1 .. numRels]
                                ]
                        return (TransferWithSchedule addr releases, transferWithScheduleEnergyCost (transferWithSchedulePayloadSize (fromIntegral numRels)) (fromIntegral numRels) numKeys)
                let
                    atrPayload = encodePayload payload
                    atrHeader =
                        TransactionHeader
                            { thSender = dropAccount,
                              thNonce = nanNonce nonce,
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
                runGRPC (doGetAccInfo addr) $ \(lastFinNonce, lastFinAmt) -> do
                    let trHash = getHash (NormalTransaction transaction) :: TransactionHash
                    runGRPC (doIsTransactionOK trHash) $ \case
                        True -> sendResponse (object ["submissionId" .= trHash])
                        False
                            | lastFinAmt < dropAmount + 1000000 -> -- FIXME: This is outdated.
                                do
                                    $(logError) "GTU drop account has insufficient funds"
                                    configErr
                            | lastFinNonce > thNonce (atrHeader transaction) ->
                                do
                                    -- Given the nonce, the transaction is no good. Delete and try again.
                                    runDB $ delete key
                                    tryDrop addr dropType gtuDropData
                            | otherwise ->
                                do
                                    currentTime <- liftIO $ round <$> getPOSIXTime
                                    if thExpiry (atrHeader transaction) < TransactionTime currentTime
                                        then do
                                            runDB $ delete key
                                            tryDrop addr dropType gtuDropData
                                        else sendTransaction transaction

getGlobalFileR :: Handler TypedContent
getGlobalFileR = toTypedContent . globalInfo <$> getYesod

-- Queries the transaction database and the GRPC,
-- then if both succeed checks that the last final block is less than `healthTolerance` seconds old.
getHealthR :: Handler TypedContent
getHealthR =
    runGRPC doGetBlockInfo $ \case
        Nothing -> do
            $(logError) "Could not get response from GRPC."
            sendResponse $ object ["healthy" .= False, "reason" .= ("Could not get response from GRPC." :: String), "version" .= showVersion version]
        Just lastFinalBlockInfo -> do
            $(logInfo) "Successfully got best block info."
            _ :: [(Entity Entry, Entity Summary)] <- runDB $
                E.select $
                    E.from $ \val -> do
                        E.limit 1
                        return val
            currentTime <- liftIO Clock.getCurrentTime
            Proxy{..} <- getYesod
            if (Clock.diffUTCTime currentTime (biBlockSlotTime lastFinalBlockInfo)) < (Clock.secondsToNominalDiffTime $ fromIntegral healthTolerance)
                then sendResponse $ object ["healthy" .= True, "lastFinalTime" .= (biBlockSlotTime lastFinalBlockInfo), "version" .= showVersion version]
                else sendResponse $ object ["healthy" .= False, "reason" .= ("The last final block is too old." :: String), "lastFinalTime" .= (biBlockSlotTime lastFinalBlockInfo), "version" .= showVersion version]
  where
    doGetBlockInfo = do
        res <- getBlockInfo LastFinal
        return $ case getResponseValueAndHeaders res of
            -- The last final block was found.
            Right (val, hds) -> StatusOk $ GRPCResponse hds $ fmap Just val
            -- The last final block was NOT found, or an error occurred.
            Left _ -> StatusOk $ GRPCResponse [] $ Right Nothing

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
    runGRPCWithCustomError
        (Just (StatusNotOkError NOT_FOUND, Just notFound404, Nothing, Just $ EMErrorResponse NotFound))
        ( do
            res <- getPoolInfo LastFinal (BakerId $ AccountIndex bid)
            case getResponseValueAndHeaders res of
                Left err -> return err
                Right (val, hds) -> do
                    lf <- getBlockHashHeader hds
                    let v = do
                            pStatus <- val
                            return (pStatus, lf)
                    return $ StatusOk $ GRPCResponse hds v
        )
        $ \(pStatus, lf) -> do
            $(logInfo) "Successfully got baker pool status."
            case pStatus of
                bps@BakerPoolStatus{..} ->
                    case psBakerStakePendingChange of
                        PPCNoChange -> sendResponse $ toJSON pStatus
                        PPCReduceBakerCapital{..} -> runGRPC (getRewardPeriodLength lf) $ \case
                            Nothing -> sendResponse $ toJSON pStatus
                            -- if there is a pending change we add the estimated change time to the response object.
                            -- The way this is done is to patch the returned JSON Value. This is not ideal, but it seems preferrable
                            -- to introducing a new type just for this.
                            Just rewardEpochs -> runGRPC (getParameters lf) $ \(nextPaydayTime, epochDuration) -> do
                                let r =
                                        object
                                            [ "pendingChangeType" .= String "ReduceBakerCapital",
                                              "bakerEquityCapital" .= ppcBakerEquityCapital,
                                              "effectiveTime" .= ppcEffectiveTime,
                                              "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs ppcEffectiveTime
                                            ]
                                case AE.toJSON bps of
                                    AE.Object o -> sendResponse (AE.toJSON (KM.insert "bakerStakePendingChange" r o))
                                    _ -> sendResponse $ toJSON pStatus
                        PPCRemovePool{..} -> runGRPC (getRewardPeriodLength lf) $ \case
                            Nothing -> sendResponse $ toJSON pStatus
                            Just rewardEpochs -> runGRPC (getParameters lf) $ \(nextPaydayTime, epochDuration) -> do
                                let r =
                                        object
                                            [ "pendingChangeType" .= String "RemovePool",
                                              "effectiveTime" .= ppcEffectiveTime,
                                              "estimatedChangeTime" .= firstPaydayAfter nextPaydayTime epochDuration rewardEpochs ppcEffectiveTime
                                            ]
                                case AE.toJSON bps of
                                    AE.Object o -> sendResponse (AE.toJSON (KM.insert "bakerStakePendingChange" r o))
                                    _ -> sendResponse $ toJSON pStatus
                PassiveDelegationStatus{} -> sendResponse $ toJSON pStatus
  where
    getParameters :: BlockHash -> ClientMonad IO (GRPCResult (Either String (UTCTime, Duration)))
    getParameters bh = do
        rewardStatus <- getTokenomicsInfo (Given bh)
        case getResponseValueAndHeaders rewardStatus of
            Left errRes -> return errRes
            Right (val, hds) -> do
                consensusInfo <- getConsensusInfo
                case getResponseValueAndHeaders consensusInfo of
                    Left errRes' -> return errRes'
                    Right (val', hds') -> do
                        let v = do
                                rStatus <- val
                                cStatus <- val'
                                return (rsNextPaydayTime rStatus, csEpochDuration cStatus)
                        return $ StatusOk $ GRPCResponse (hds ++ hds') v

getChainParametersR :: Handler TypedContent
getChainParametersR =
    runGRPC doGetParameters $ \(v :: Value) -> do
        $(logInfo) "Successfully got chain parameters."
        sendResponse v
  where
    doGetParameters = do
        bcpRes <- getBlockChainParameters LastFinal
        return $ case getResponseValueAndHeaders bcpRes of
            Left errRes -> errRes
            Right (Left err, hds) -> StatusOk $ GRPCResponse hds $ Left err
            Right (Right (EChainParametersAndKeys ecpParams _), hds) -> StatusOk $ GRPCResponse hds $ Right $ toJSON ecpParams

getNextPaydayR :: Handler TypedContent
getNextPaydayR =
    runGRPCWithCustomError
        (Just (InvariantError, Just badRequest400, Just RequestInvalid, Just $ EMErrorResponse $ InvalidArgs []))
        doGetParameters
        $ \(v :: UTCTime) -> do
            let timestampObject = object ["nextPaydayTime" .= v]
            $(logInfo) "Successfully got next payday."
            sendResponse $ toJSON timestampObject
  where
    doGetParameters = do
        rewardStatusRes <- getTokenomicsInfo LastFinal
        case getResponseValueAndHeaders rewardStatusRes of
            Left errRes -> return errRes
            Right (Left err, hds) -> return $ StatusOk $ GRPCResponse hds $ Left err
            Right (Right rewardStatus, hds) -> do
                case rewardStatus of
                    RewardStatusV0{} ->
                        return $ StatusOk $ GRPCResponse hds $ Left "Received unexpected 'RewardStatusV0' variant in response payload, expected 'RewardStatusV1'."
                    RewardStatusV1{..} ->
                        return $ StatusOk $ GRPCResponse hds $ Right rsNextPaydayTime

getPassiveDelegationR :: Handler TypedContent
getPassiveDelegationR =
    runGRPC (getPassiveDelegationInfo LastFinal) $ \v -> do
        $(logInfo) "Successfully got baker pool status."
        sendResponse $ toJSON v

matchesVersion :: Word -> Maybe ForcedUpdateConfig -> AE.Value
matchesVersion _ Nothing = AE.object ["status" AE..= String "ok"]
matchesVersion queryVersion (Just ForcedUpdateConfig{..})
    | inRanges fucForceUpdate queryVersion =
        AE.object
            [ "status" AE..= String "needsUpdate",
              "url" AE..= fucURL
            ]
    | inRanges fucSuggestUpdate queryVersion =
        AE.object
            [ "status" AE..= String "warning",
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
    -- | Forced update configuration for the Android variant of the app.
    Maybe ForcedUpdateConfig ->
    -- | Forced update configuration for the IOS variant of the app.
    Maybe ForcedUpdateConfig ->
    Handler TypedContent
getAppSettingsWorker fucAndroid fucIOS = do
    mPlatform <- lookupGetParam "platform"
    mVersion <- lookupGetParam "appVersion"
    case (mPlatform, readMaybe . Text.unpack =<< mVersion) of
        (Just platform, Just queryVersion) -> do
            if platform == "android"
                then sendResponse (matchesVersion queryVersion fucAndroid)
                else
                    if platform == "ios"
                        then sendResponse (matchesVersion queryVersion fucIOS)
                        else respond400Error (EMParseError "'param' should either be 'android' or 'ios'") RequestInvalid
        (Just _, Nothing) -> respond400Error (EMParseError "'version' parameter is not present or readable. It must be a non-negative integer.") RequestInvalid
        (Nothing, Just _) -> respond400Error (EMParseError "'platform' field is not present.") RequestInvalid
        (Nothing, Nothing) -> respond400Error (EMParseError "'platform' field is not present and 'version' field is either not present or not readable.") RequestInvalid

getEpochLengthR :: Handler TypedContent
getEpochLengthR =
    runGRPC getConsensusInfo $ \cInfo -> do
        let epochLengthObject = object ["epochLength" .= csEpochDuration cInfo]
        $(logOther "Trace") "Successfully got epoch length."
        sendResponse $ toJSON epochLengthObject
