{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Proxy
import Yesod
import Database.Persist.Postgresql
import qualified Network.Wai.Handler.Warp
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.HashMap.Strict as HM
import Concordium.ID.Types (AccountAddress, KeyIndex)
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
  pcGTUAccountFile :: FilePath,
  pcGlobal :: FilePath,
  pcIpInfo :: FilePath
}

parser :: ParserInfo ProxyConfig
parser = info (helper <*> parseProxyConfig)
          (fullDesc <> progDesc "Concordium wallet proxy server.")
  where
    parseProxyConfig = mkProxyConfig
      <$> backendParser
      <*> strOption (long "db" <> metavar "STR" <> help "database connection string")
      <*> strOption (long "drop-account" <> metavar "FILE" <> help "file with GTU drop account credentials")
      <*> strOption (long "global" <> metavar "FILE" <> help "File with global parameters.")
      <*> strOption (long "ip-data" <> metavar "FILE" <> help "File with public and information on the identity providers, together with metadata.")
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

accountParser :: AE.Value -> AE.Parser (AccountAddress, [(KeyIndex, KeyPair)])
accountParser = AE.withObject "Account keys" $ \v -> do
          accountAddr <- v AE..: "address"
          accountData <- v AE..: "accountData"
          keyMap <- accountData AE..: "keys"
          return (accountAddr, HM.toList keyMap)

main :: IO ()
main = do
  ProxyConfig{..} <- execParser parser
  let logm s = runStderrLoggingT ($logDebug ("[GRPC]: " <> s))
  keyFile <- LBS.readFile pcGTUAccountFile
  Right globalInfo <- AE.eitherDecode' <$> LBS.readFile pcGlobal
  Right ipInfo <- AE.eitherDecode' <$> LBS.readFile pcIpInfo
  let getKeys = AE.eitherDecode' keyFile >>= AE.parseEither accountParser
  case getKeys of
    Left err -> die $ "Cannot parse account keys: " ++ show err
    Right (dropAccount, dropKeys) ->
      runStderrLoggingT $ withPostgresqlPool pcDBConnString 10 $ \dbConnectionPool -> liftIO $ do
        runSqlPool (runMigration migrateGTURecipient) dbConnectionPool
        runExceptT (mkGrpcClient pcGRPC (Just logm)) >>= \case
          Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
          Right cfg ->
            runSite 3000 "0.0.0.0" Proxy{grpcEnvData=cfg, ..}
