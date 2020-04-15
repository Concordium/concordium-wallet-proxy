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
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Fail(MonadFail)
import Data.Aeson(withObject, object, (.=), fromJSON, Result(..))
import Data.Aeson.Types(parse)
import Data.Aeson.Parser(json')
import Data.Conduit(connect)
import qualified Data.Serialize as S
import Data.Conduit.Attoparsec  (sinkParserEither)
import Network.HTTP.Types(badRequest400, badGateway502)
import Yesod hiding (InternalError)

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Client.TransactionStatus
import Concordium.Client.GRPC

data Proxy = Proxy !EnvData
instance Yesod Proxy

defaultNetId :: Int
defaultNetId = 100

mkYesod "Proxy" [parseRoutes|
/accNonce/#Text AccountNonceR GET
/submissionStatus/#Text SubmissionStatusR GET
/submitCredential/ CredentialR PUT
/submitTransfer/ TransferR PUT
|]

data ErrorCode = InternalError | RequestInvalid
    deriving(Eq, Show, Enum)

respond400Error :: ToJSON a => a -> ErrorCode -> Handler TypedContent
respond400Error err code =
  sendResponseStatus badRequest400 $
    object ["errorMessage" .= err,
            "error" .= fromEnum code
           ]

runGRPC :: ClientMonad IO (Either String a) -> (a -> Handler TypedContent) -> Handler TypedContent
runGRPC c k = do
  Proxy cfg <- getYesod
  liftIO (runClient cfg c) >>= \case
    Left err -> do
      $(logError) $ "Internal error accessing GRPC endpoint: " <> Text.pack (show err)
      sendResponseStatus badGateway502 $ object [
        "errorMessage" .= show err,
        "error" .= fromEnum InternalError
        ]
    Right (Left err) -> respond400Error err RequestInvalid
    Right (Right a) -> k a

getAccountNonceR :: Text -> Handler TypedContent
getAccountNonceR addrText =
    runGRPC (getNextAccountNonce addrText) $ \v -> do
      $(logInfo) "Successfully got nonce."
      sendResponse v

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
