{-# LANGUAGE OverloadedStrings #-}
module Main where

import Proxy
import Yesod
import qualified Network.Wai.Handler.Warp

import System.Exit(die)
import Control.Monad.Except

import Concordium.Client.GRPC
import Concordium.Client.Commands
import Options.Applicative

data DBOptions = DBOptions {
  dbConnString :: String
  }

dbOptions :: Parser DBOptions
dbOptions = do
  let dbString = strOption (long "db" <> metavar "STR")
  DBOptions <$> dbString

parser :: ParserInfo (Backend, DBOptions)
parser = info (helper <*> ((,) <$> backendParser <*> dbOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")


defaultConfig :: GrpcConfig
defaultConfig = GrpcConfig "localhost" 11109 Nothing

-- dbConnString :: String
-- dbConnString = "host=localhost port=5432 user=concordium dbname=concordium password=concordium"

runSite :: YesodDispatch site => Int -> Network.Wai.Handler.Warp.HostPreference -> site -> IO ()
runSite port host site = do

    toWaiApp site >>= Network.Wai.Handler.Warp.runSettings (
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setServerName "Concordium-wallet-proxy" $
        Network.Wai.Handler.Warp.setHost host $
        Network.Wai.Handler.Warp.defaultSettings)

main :: IO ()
main = do
  (backend, dbOptions) <- execParser parser
  runExceptT (mkGrpcClient defaultConfig) >>= \case
    Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
    Right cfg ->
      runSite 3000 "0.0.0.0" (Proxy cfg)
