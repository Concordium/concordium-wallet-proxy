{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Proxy where

import Control.Monad.IO.Class   (liftIO)
import Data.Aeson(Value(..), object, (.=), fromJSON, Result(..))
import Data.Aeson.Parser        (json')
import Data.Conduit(connect)
import Data.Conduit.Attoparsec  (sinkParserEither)
import Network.HTTP.Types(badRequest400, badGateway502)
import Yesod

import Data.Text(Text)
import qualified Data.Text as Text

import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Client.GRPC

data Proxy = Proxy !EnvData
instance Yesod Proxy

defaultNetId :: Int
defaultNetId = 100

mkYesod "Proxy" [parseRoutes|
/accNonce/#Text AccountNonceR GET
/submissionStatus/#Text SubmissionStatusR GET
/submitCredential/ CredentialR PUT
|]

respond400Error :: ToJSON a => a -> Handler TypedContent
respond400Error err = sendResponseStatus badRequest400 $ object ["error" .= err]

runGRPC :: ClientMonad IO (Either String a) -> (a -> Handler TypedContent) -> Handler TypedContent
runGRPC c k = do
  Proxy cfg <- getYesod
  liftIO (runClient cfg c) >>= \case
    Left err -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack (show err)
      sendResponseStatus badGateway502 Null
    Right (Left err) -> respond400Error err
    Right (Right a) -> k a

getAccountNonceR :: Text -> Handler TypedContent
getAccountNonceR addrText =
  case addressFromText addrText of
    Left err -> respond400Error err
    Right _ -> do
      runGRPC (getNextAccountNonce addrText) $ \v -> do
          $(logInfo) "Successfully got nonce."
          sendResponse v

putCredentialR :: Handler TypedContent
putCredentialR = 
  connect rawRequestBody (sinkParserEither json') >>= \case
    Left err -> respond400Error (show err)
    Right credJSON ->
      case fromJSON credJSON of
        Error err -> respond400Error err
        Success cdi -> do
          runGRPC (sendTransactionToBaker (CredentialDeployment cdi) defaultNetId) $ \case
            False -> do
              $(logError) $ "Credential rejected by node."
              sendResponse Null
            True -> 
              sendResponse (object ["submissionid" .= (getHash (CredentialDeployment cdi) :: TransactionHash)])

-- Get the status of the submission.
getSubmissionStatusR :: Text -> Handler TypedContent
getSubmissionStatusR submissionId =
  runGRPC (getTransactionStatus submissionId) sendResponse
