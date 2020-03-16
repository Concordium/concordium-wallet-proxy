module Main where

import Proxy
import Yesod

import System.Exit(die)
import Control.Monad.Except

import Concordium.Client.GRPC

defaultConfig :: GrpcConfig
defaultConfig = GrpcConfig "localhost" 11109 Nothing

dbConnString :: String
dbConnString = "host=localhost port=5432 user=concordium dbname=concordium password=concordium"

main :: IO ()
main =
  runExceptT (mkGrpcClient defaultConfig) >>= \case
    Left err -> die $ "Cannot connect to GRPC endpoint: " ++ show err
    Right cfg -> warp 3000 (Proxy cfg)
