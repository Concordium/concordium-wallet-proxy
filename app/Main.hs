{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String
import Database.Persist.Postgresql
import qualified Logging
import qualified Network.Wai.Handler.Warp
import Proxy
import Yesod

import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import System.Exit (die)

import Concordium.Client.Commands as CMDS
import Concordium.Client.GRPC2
import Concordium.Client.Runner.Helper (getResponseValue)
import Concordium.Client.Utils
import Concordium.Common.Version (Version (Version), Versioned (Versioned))
import Concordium.Crypto.SignatureScheme (KeyPair)
import Concordium.ID.Types (AccountAddress, AccountThreshold, CredentialIndex, KeyIndex, SignatureThreshold)
import Concordium.Types (Amount)
import Concordium.Types.Queries as Types
import Data.Range.Parser
import Options.Applicative

data ProxyConfig = ProxyConfig
    { pcGRPC :: GrpcConfig,
      pcPort :: Int,
      pcDBConnString :: ByteString,
      -- | Account file used for GTU drop. Only used on stagenet and testnet.
      pcGTUAccountFile :: Maybe FilePath,
      -- | Amount to give out.
      pcGTUDropAmount :: Amount,
      pcForcedUpdateConfigFileV0 :: Maybe FilePath,
      pcForcedUpdateConfigFileV1 :: Maybe FilePath,
      pcHealthTolerance :: Maybe Int,
      pcIpInfo :: FilePath,
      pcIpInfoV1 :: FilePath,
      pcIpInfoV2 :: FilePath,
      logLevel :: Logging.LogLevel,
      tcVersion :: Maybe String,
      tcUrl :: Maybe String
    }

parser :: ParserInfo ProxyConfig
parser =
    info
        (helper <*> parseProxyConfig)
        (fullDesc <> progDesc "Concordium wallet proxy server.")
  where
    parseProxyConfig =
        mkProxyConfig
            <$> backendParser
            <*> optional (option auto (long "grpc-timeout" <> value 15 <> showDefault <> metavar "TIMEOUT" <> help "Timeout of grpc requests."))
            <*> option auto (long "port" <> metavar "PORT" <> help "Port number to run the server on" <> showDefault <> value 3000)
            <*> strOption (long "db" <> metavar "STR" <> help "database connection string")
            <*> optional (strOption (long "drop-account" <> metavar "FILE" <> help "file with CCD drop account credentials (only used for stagenet and testnet)."))
            <*> option (eitherReader amountFromStringInform) (long "drop-amount" <> metavar "CCD-AMOUNT" <> help "Amount of CCDs to drop upon request." <> value 2_000_000_000)
            <*> optional (strOption (long "forced-update-config-v0" <> metavar "FILE" <> help "file with the version configuration for forced app updates for the old mobile wallet."))
            <*> optional (strOption (long "forced-update-config-v1" <> metavar "FILE" <> help "file with the version configuration for forced app updates for the new mobile wallet."))
            <*> optional (option auto (long "health-tolerance" <> metavar "SECONDS" <> help "the maximum tolerated age of the last final block in seconds before the health query returns false."))
            <*> strOption (long "ip-data" <> metavar "FILE" <> help "File with public and private information on the identity providers, together with metadata.")
            <*> strOption (long "ip-data-v1" <> metavar "FILE" <> help "File with public and private information on the identity providers (excluding Company ID providers) for the flow without initial accounts, together with metadata.")
            <*> strOption (long "ip-data-v2" <> metavar "FILE" <> help "File with public and private information on the identity providers (including Company ID providers) for the flow without initial accounts, together with metadata.")
            <*> option (eitherReader Logging.logLevelFromString) (long "log-level" <> metavar "LOGLEVEL" <> value Logging.LLOff <> showDefault <> help "Log level. Can be one of either 'off', 'error', 'warning', 'info', 'debug' or 'trace'.")
            <*> optional (strOption (long "tc-version" <> metavar "STRING" <> help "Version of terms and conditions in effect."))
            <*> optional (strOption (long "tc-url" <> metavar "URL" <> help "Link to the terms and conditions."))

    mkProxyConfig backend timeout =
        ProxyConfig $
            GrpcConfig
                (CMDS.grpcHost backend)
                (CMDS.grpcPort backend)
                (CMDS.grpcTarget backend)
                (CMDS.grpcRetryNum backend)
                (Just (fromMaybe 15 timeout))
                (CMDS.grpcUseTls backend)

runSite :: (YesodDispatch site) => Int -> String -> site -> IO ()
runSite port host site = do
    toWaiApp site
        >>= Network.Wai.Handler.Warp.runSettings
            ( Network.Wai.Handler.Warp.setPort port $
                Network.Wai.Handler.Warp.setServerName "Concordium-wallet-proxy" $
                    Network.Wai.Handler.Warp.setHost (fromString host) $
                        Network.Wai.Handler.Warp.defaultSettings
            )

accountParser :: AE.Value -> AE.Parser (AccountAddress, [(CredentialIndex, [(KeyIndex, KeyPair)])])
accountParser = AE.withObject "Account keys" $ \v -> do
    accountAddr <- v AE..: "address"
    accountKeys <- v AE..: "accountKeys"
    credentialsMap <- accountKeys AE..: "keys"
    threshold :: AccountThreshold <- accountKeys AE..: "threshold"
    keysMap <- forM credentialsMap $ AE.withObject "Credential Keys" $ \obj -> do
        ks <- obj AE..: "keys"
        keyThreshold :: SignatureThreshold <- obj AE..: "threshold"
        return $ take (fromIntegral keyThreshold) $ Map.toAscList ks
    return (accountAddr, take (fromIntegral threshold) (Map.toAscList keysMap))

forcedUpdateConfigParser :: AE.Value -> AE.Parser (Maybe ForcedUpdateConfig)
forcedUpdateConfigParser = AE.withObject "Forced update config" $ \v -> do
    forceString <- v AE..:? "forceUpdateVersions" AE..!= ""
    suggestString <- v AE..:? "suggestUpdateVersions" AE..!= ""
    mfucURL <- v AE..:? "url"
    mfucSuggestURL <- v AE..:? "suggestUrl"
    case parseRanges forceString of
        Left err -> fail $ "Invalid forced update range: " ++ show err
        Right fucForceUpdate -> case parseRanges suggestString of
            Left err -> fail $ "Invalid suggest update range: " ++ show err
            Right fucSuggestUpdate ->
                case mfucURL of
                    Just fucURL -> return $ Just ForcedUpdateConfig{fucSuggestURL = fromMaybe fucURL mfucSuggestURL, ..}
                    Nothing
                        | null fucForceUpdate && null fucSuggestUpdate -> return Nothing
                        | otherwise -> fail "If non-empty ranges are given then the URL must be present."

-- | Return a pair of configurations, the first one for iOS the second for Android.
forcedUpdateParser :: AE.Value -> AE.Parser (Maybe ForcedUpdateConfig, Maybe ForcedUpdateConfig)
forcedUpdateParser = AE.withObject "Forced update configs" $ \v -> do
    ios <-
        v AE..:? "ios" >>= \case
            Nothing -> return Nothing
            Just vv -> forcedUpdateConfigParser vv
    android <-
        v AE..:? "android" >>= \case
            Nothing -> return Nothing
            Just vv -> forcedUpdateConfigParser vv
    return (ios, android)

main :: IO ()
main = do
    ProxyConfig{..} <- execParser parser
    let filterL = Logging.filterL logLevel
    let logm s = runStderrLoggingT $ filterL ($logDebug ("[GRPC]: " <> s))
    let healthTolerance = fromMaybe 300 pcHealthTolerance -- use 5 minutes as default health tolerance
    gtuDropData <- case pcGTUAccountFile of
        Nothing -> return Nothing
        Just accFile -> do
            keyFile <- LBS.readFile accFile
            let getKeys = AE.eitherDecode' keyFile >>= AE.parseEither accountParser
            case getKeys of
                Left err -> die $ "Cannot parse account keys: " ++ show err
                Right (dropAccount, dropKeys) -> return . Just $ GTUDropData{dropAmount = pcGTUDropAmount, ..}
    (forcedUpdateConfigIOSV0, forcedUpdateConfigAndroidV0) <- case pcForcedUpdateConfigFileV0 of
        Nothing -> return (Nothing, Nothing)
        Just fuFileName -> do
            fuFile <- LBS.readFile fuFileName
            let getUpdateConfig = AE.eitherDecode' fuFile >>= AE.parseEither forcedUpdateParser
            case getUpdateConfig of
                Left err -> die $ "Cannot parse forced update config V0: " ++ show err
                Right cfg -> return cfg
    (forcedUpdateConfigIOSV1, forcedUpdateConfigAndroidV1) <- case pcForcedUpdateConfigFileV1 of
        Nothing -> return (Nothing, Nothing)
        Just fuFileName -> do
            fuFile <- LBS.readFile fuFileName
            let getUpdateConfig = AE.eitherDecode' fuFile >>= AE.parseEither forcedUpdateParser
            case getUpdateConfig of
                Left err -> die $ "Cannot parse forced update config V1: " ++ show err
                Right cfg -> return cfg
    Right ipInfo <- AE.eitherDecode' <$> LBS.readFile pcIpInfo
    Right ipInfoV1 <- AE.eitherDecode' <$> LBS.readFile pcIpInfoV1
    Right ipInfoV2 <- AE.eitherDecode' <$> LBS.readFile pcIpInfoV2
    let host = "0.0.0.0"
    runStderrLoggingT . filterL $ do
        $logDebug ("Using iOS V0 update config: " <> fromString (show forcedUpdateConfigIOSV0))
        $logDebug ("Using Android V0 update config: " <> fromString (show forcedUpdateConfigAndroidV0))
        $logDebug ("Using iOS V1 update config: " <> fromString (show forcedUpdateConfigIOSV1))
        $logDebug ("Using Android V1 update config: " <> fromString (show forcedUpdateConfigAndroidV1))
        $logDebug ("Using logLevel: " <> fromString (show logLevel))
        $logDebug ("Running server on: " <> fromString host <> ":" <> fromString (show pcPort))
    runStderrLoggingT . filterL $ withPostgresqlPool pcDBConnString 10 $ \dbConnectionPool -> liftIO $ do
        -- do not care about the gtu receipients database if gtu drop is not enabled
        when (isJust gtuDropData) $ runSqlPool (runMigration migrateGTURecipient) dbConnectionPool
        runExceptT (mkGrpcClient pcGRPC (Just logm)) >>= \case
            Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
            Right cfg -> do
                -- The getCryptographicParameters returns a versioned cryptographic parameters object, which is what we need.
                -- Because these parameters do not change we only look them up on startup, and store them.
                runClient cfg (getCryptographicParameters Types.LastFinal) >>= \case
                    Left err -> die $ "Cannot obtain cryptographic parameters due to network error: " ++ show err
                    Right res -> case getResponseValue res of
                        Right cParams -> do
                            let globalInfo = toJSON $ Versioned (Version 0) cParams
                            runSite pcPort host Proxy{grpcEnvData = cfg, ..}
                        Left (_, err) -> die $ "Cannot obtain cryptographic parameters due to error: " ++ err
