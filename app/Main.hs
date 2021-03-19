{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Proxy
import Yesod
import Database.Persist.Postgresql
import qualified Network.Wai.Handler.Warp
import Data.ByteString(ByteString)
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

data ProxyConfig = ProxyConfig {
  pcGRPC :: GrpcConfig,
  pcDBConnString :: ByteString,
  -- | Account file used for GTU drop. Only used on stagenet and testnet.
  pcGTUAccountFile :: Maybe FilePath,
  pcIpInfo :: FilePath
}

parser :: ParserInfo ProxyConfig
parser = info (helper <*> parseProxyConfig)
          (fullDesc <> progDesc "Concordium wallet proxy server.")
  where
    parseProxyConfig = mkProxyConfig
      <$> backendParser
      <*> strOption (long "db" <> metavar "STR" <> help "database connection string")
      <*> optional (strOption (long "drop-account" <> metavar "FILE" <> help "file with GTU drop account credentials (only used for stagenet and testnet)."))
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

main :: IO ()
main = do
  ProxyConfig{..} <- execParser parser
  let logm s = runStderrLoggingT ($logDebug ("[GRPC]: " <> s))
  gtuDropData <- case pcGTUAccountFile of
    Nothing -> return Nothing
    Just accFile -> do
      keyFile <- LBS.readFile accFile
      let getKeys = AE.eitherDecode' keyFile >>= AE.parseEither accountParser
      case getKeys of
        Left err -> die $ "Cannot parse account keys: " ++ show err
        Right (dropAccount, dropKeys) -> return . Just $ GTUDropData {..}
  Right ipInfo <- AE.eitherDecode' <$> LBS.readFile pcIpInfo
  runStderrLoggingT $ withPostgresqlPool pcDBConnString 10 $ \dbConnectionPool -> liftIO $ do
    runSqlPool (runMigration migrateGTURecipient) dbConnectionPool
    runExceptT (mkGrpcClient pcGRPC (Just logm)) >>= \case
      Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
      Right cfg -> do
        -- The getCryptographicParameters returns a versioned cryptographic parameters object, which is what we need.
        -- Because these parameters do not change we only look them up on startup, and store them.
        runClient cfg (withLastFinalBlockHash Nothing getCryptographicParameters) >>= \case
          Left err -> die $ "Cannot obtain cryptographic parameters due to network error: " ++ show err
          Right (Left err) -> die $ "Cannot obtain cryptographic parameters due to unexpected response: " ++ err
          Right (Right globalInfo) -> runSite 3000 "0.0.0.0" Proxy{grpcEnvData=cfg, ..}
