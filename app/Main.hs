{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Proxy
import Yesod
import Database.Persist.Postgresql
import qualified Network.Wai.Handler.Warp
import Data.ByteString(ByteString)
import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.Map.Strict as Map
import Concordium.ID.Types (AccountAddress, KeyIndex, AccountThreshold, SignatureThreshold, CredentialIndex)
import Concordium.Crypto.SignatureScheme (KeyPair)

import System.Exit(die)
import Control.Monad.Except
import Control.Monad.Logger

import Concordium.Client.GRPC
import Concordium.Client.Commands as CMDS
import Options.Applicative
import Data.Range.Parser

data ProxyConfig = ProxyConfig {
  pcGRPC :: GrpcConfig,
  pcDBConnString :: ByteString,
  -- | Account file used for GTU drop. Only used on stagenet and testnet.
  pcGTUAccountFile :: Maybe FilePath,
  pcForcedUpdateConfigFile :: Maybe FilePath,
  pcHealthTolerance :: Maybe Int,
  pcIpInfo :: FilePath
}

parser :: ParserInfo ProxyConfig
parser = info (helper <*> parseProxyConfig)
          (fullDesc <> progDesc "Concordium wallet proxy server.")
  where
    parseProxyConfig = mkProxyConfig
      <$> backendParser
      <*> strOption (long "db" <> metavar "STR" <> help "database connection string")
      <*> optional (strOption (long "drop-account" <> metavar "FILE" <> help "file with CCD drop account credentials (only used for stagenet and testnet)."))
      <*> optional (strOption (long "forced-update-config" <> metavar "FILE" <> help "file with the version configuration for forced app updates."))
      <*> optional (option auto (long "health-tolerance" <> metavar "SECONDS" <> help "the maximum tolerated age of the last final block in seconds before the health query returns false."))
      <*> strOption (long "ip-data" <> metavar "FILE" <> help "File with public and private information on the identity providers, together with metadata.")
    mkProxyConfig backend = ProxyConfig $ GrpcConfig
                              (CMDS.grpcHost backend)
                              (CMDS.grpcPort backend)
                              (CMDS.grpcAuthenticationToken backend)
                              (CMDS.grpcTarget backend)
                              (CMDS.grpcRetryNum backend)
                              (Just 30)

runSite :: YesodDispatch site => Int -> Network.Wai.Handler.Warp.HostPreference -> site -> IO ()
runSite port host site = do

    toWaiApp site >>= Network.Wai.Handler.Warp.runSettings (
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setServerName "Concordium-wallet-proxy" $
        Network.Wai.Handler.Warp.setHost host $
        Network.Wai.Handler.Warp.defaultSettings)

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
  case parseRanges forceString of
    Left err -> fail $ "Invalid forced update range: " ++ show err
    Right fucForceUpdate -> case parseRanges suggestString of
      Left err -> fail $ "Invalid suggest update range: " ++ show err
      Right fucSuggestUpdate ->
        case mfucURL of
          Just fucURL -> return $ Just ForcedUpdateConfig{..}
          Nothing | null fucForceUpdate && null fucSuggestUpdate -> return Nothing
                  | otherwise -> fail "If non-empty ranges are given then the URL must be present."

-- |Return a pair of configurations, the first one for iOS the second for Android.
forcedUpdateParser :: AE.Value -> AE.Parser (Maybe ForcedUpdateConfig, Maybe ForcedUpdateConfig)
forcedUpdateParser = AE.withObject "Forced update configs" $ \v -> do
  ios <- v AE..:? "ios" >>= \case
    Nothing -> return Nothing
    Just vv -> forcedUpdateConfigParser vv
  android <- v AE..:? "android" >>= \case
    Nothing -> return Nothing
    Just vv -> forcedUpdateConfigParser vv
  return (ios, android)

main :: IO ()
main = do
  ProxyConfig{..} <- execParser parser
  let logm s = runStderrLoggingT ($logDebug ("[GRPC]: " <> s))
  let healthTolerance = fromMaybe 300 pcHealthTolerance -- use 5 minutes as default health tolerance
  gtuDropData <- case pcGTUAccountFile of
    Nothing -> return Nothing
    Just accFile -> do
      keyFile <- LBS.readFile accFile
      let getKeys = AE.eitherDecode' keyFile >>= AE.parseEither accountParser
      case getKeys of
        Left err -> die $ "Cannot parse account keys: " ++ show err
        Right (dropAccount, dropKeys) -> return . Just $ GTUDropData {..}
  (forcedUpdateConfigIOS, forcedUpdateConfigAndroid) <- case pcForcedUpdateConfigFile of
    Nothing -> return (Nothing, Nothing)
    Just fuFileName -> do
      fuFile <- LBS.readFile fuFileName
      let getUpdateConfig = AE.eitherDecode' fuFile >>= AE.parseEither forcedUpdateParser
      case getUpdateConfig of
        Left err -> die $ "Cannot parse forced update config: " ++ show err
        Right cfg -> return cfg
  Right ipInfo <- AE.eitherDecode' <$> LBS.readFile pcIpInfo
  runStderrLoggingT $ do
    $logDebug ("Using iOS update config: " <> fromString (show forcedUpdateConfigIOS))
    $logDebug ("Using Android update config: " <> fromString (show forcedUpdateConfigAndroid))
  runStderrLoggingT $ withPostgresqlPool pcDBConnString 10 $ \dbConnectionPool -> liftIO $ do
    -- do not care about the gtu receipients database if gtu drop is not enabled
    when (isJust gtuDropData) $ runSqlPool (runMigration migrateGTURecipient) dbConnectionPool
    runExceptT (mkGrpcClient pcGRPC (Just logm)) >>= \case
      Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
      Right cfg -> do
        -- The getCryptographicParameters returns a versioned cryptographic parameters object, which is what we need.
        -- Because these parameters do not change we only look them up on startup, and store them.
        runClient cfg (withLastFinalBlockHash Nothing getCryptographicParameters) >>= \case
          Left err -> die $ "Cannot obtain cryptographic parameters due to network error: " ++ show err
          Right (Left err) -> die $ "Cannot obtain cryptographic parameters due to unexpected response: " ++ err
          Right (Right globalInfo) -> runSite 3000 "0.0.0.0" Proxy{grpcEnvData=cfg, ..}
