{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements backend functionality for integrating the Transak on-ramp in wallets.
--  In particular, it provides functionality for invoking the "Create Widget URL" API.
--  See: https://docs.transak.com/reference/create-widget-url#/
module Transak where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URI

import Concordium.Types

-- | Configuration for the Transak on-ramp.
data TransakConfig = TransakConfig
    { -- | Whether to use the staging environment.
      transakUseStaging :: !Bool,
      -- | API secret set by Transak.
      transakApiSecret :: !BS.ByteString,
      -- | API key set by Transak.
      transakApiKey :: !Text,
      -- | Referrer domain to use with Transak calls.
      transakReferrerDomain :: !Text
    }

instance AE.FromJSON TransakConfig where
    parseJSON = AE.withObject "TransakConfig" $ \v -> do
        transakUseStaging <- v AE..:? "useStaging" AE..!= False
        transakApiSecret <- encodeUtf8 <$> v AE..: "apiSecret"
        transakApiKey <- v AE..: "apiKey"
        transakReferrerDomain <- v AE..: "referrerDomain"
        return TransakConfig{..}

-- | Relative URI path for the refresh-token call.
refreshTokenURI ::
    -- | 'True' to use the staging environment, 'False' for production.
    Bool ->
    URI
refreshTokenURI False = fromJust $ parseAbsoluteURI "https://api.transak.com/partners/api/v2/refresh-token"
refreshTokenURI True = fromJust $ parseAbsoluteURI "https://api-stg.transak.com/partners/api/v2/refresh-token"

-- | Relative URI path for the create widget URL call.
createWidgetUrlURI ::
    -- | 'True' to use the staging environment, 'False' for production.
    Bool ->
    URI
createWidgetUrlURI False = fromJust $ parseAbsoluteURI "https://api-gateway.transak.com/api/v2/auth/session"
createWidgetUrlURI True = fromJust $ parseAbsoluteURI "https://api-gateway-stg.transak.com/api/v2/auth/session"

-- | Headers setting the content-type and accept to "application/json".
jsonHeaders :: [Header]
jsonHeaders = [("accept", "application/json"), ("content-type", "application/json")]

-- | A Transak access token and its expiry time.
data AccessToken = AccessToken
    { accessToken :: !BS.ByteString,
      expiresAt :: !POSIXTime
    }
    deriving (Eq, Show)

-- | A trivial access token that has already expired.
emptyAccessToken :: AccessToken
emptyAccessToken = AccessToken "" 0

-- | This creates a shared access token resource that is initially expired.
makeInitialAccessToken :: IO (MVar AccessToken)
makeInitialAccessToken = newMVar emptyAccessToken

instance AE.FromJSON AccessToken where
    parseJSON =
        AE.withObject "AccessToken" $
            (AE..: "data")
                >=> ( AE.withObject "data" $ \v -> do
                        accessToken <- encodeUtf8 <$> v AE..: "accessToken"
                        expiresAt <- v AE..: "expiresAt"
                        return AccessToken{..}
                    )

-- | This calls the "refresh access token" Transak API to get a new access token.
--  It returns the new 'AccessToken' if successful. Otherwise, it throws an 'IOException'.
--
--  Reference: https://docs.transak.com/reference/refresh-access-token
refreshAccessToken :: TransakConfig -> IO AccessToken
refreshAccessToken TransakConfig{..} = do
    let requestURI = refreshTokenURI transakUseStaging
    initialRequest <- requestFromURI requestURI
    let requestObject = AE.object ["apiKey" AE..= transakApiKey]
    let headers = ("api-secret", transakApiSecret) : jsonHeaders
    let request =
            initialRequest
                { method = "POST",
                  requestBody = RequestBodyLBS $ AE.encode requestObject,
                  requestHeaders = headers
                }
    manager <- getGlobalManager
    response <- httpLbs request manager
    unless (statusIsSuccessful $ responseStatus response) $ do
        throwIO . userError $ "Refreshing the access token was unsuccessful: " ++ show response
    let body = responseBody response
    case AE.eitherDecode body of
        Left e ->
            throwIO . userError $
                "Could not decode response when refreshing access token: " ++ e
        Right accessTokenResp -> do
            return accessTokenResp

-- | Get the Transak access token, refreshing it if it has expired, or will expire within 1 minute.
getAccessToken :: TransakConfig -> MVar AccessToken -> IO AccessToken
getAccessToken conf mvToken =
    bracketOnError
        (takeMVar mvToken)
        (tryPutMVar mvToken)
        ( \token -> do
            currentTime <- getPOSIXTime
            newToken <-
                if currentTime + 60 > expiresAt token
                    then refreshAccessToken conf
                    else return token
            putMVar mvToken newToken
            return newToken
        )

-- | Wrapper type for the widget URL returned by `createWidgetUrl`.
newtype WidgetUrlResponse = WidgetUrlResponse {widgetUrl :: URI}
    deriving (Eq, Show)

instance AE.FromJSON WidgetUrlResponse where
    parseJSON =
        AE.withObject "WidgetUrlResponse" $
            (AE..: "data")
                >=> (AE.withObject "data" $ \v -> WidgetUrlResponse <$> v AE..: "widgetUrl")

-- | This calls the "create widget URL" Transak API with the specified waleet address.
--  The parameter "disableWalletAddressForm" is set, so that Transak will not allow the user to
--  change the address. The crypto currency is set to CCD.
--
--  If successful, this will return the URI produced by the API. If the upstream server responds
--  with a non-successful code, then this will return the status code. If the response from the
--  server is successful, but the result could not be parsed, this throws an 'IOException'.
--
--  Reference: https://docs.transak.com/reference/create-widget-url#/
createWidgetUrl ::
    -- | Configuration
    TransakConfig ->
    -- | Access token
    BS.ByteString ->
    -- | Account address to use
    AccountAddress ->
    IO (Either Status URI)
createWidgetUrl TransakConfig{..} token addr = do
    let requestURI = createWidgetUrlURI transakUseStaging
    initialRequest <- requestFromURI requestURI
    let requestObject =
            AE.object
                [ "widgetParams"
                    AE..= AE.object
                        [ "apiKey" AE..= transakApiKey,
                          "cryptoCurrencyCode" AE..= ("CCD" :: Text),
                          "referrerDomain" AE..= transakReferrerDomain,
                          "walletAddress" AE..= addr,
                          "disableWalletAddressForm" AE..= True
                        ]
                ]
    let headers = ("access-token", token) : jsonHeaders
    let request =
            initialRequest
                { method = "POST",
                  requestBody = RequestBodyLBS $ AE.encode requestObject,
                  requestHeaders = headers
                }
    manager <- getGlobalManager
    response <- httpLbs request manager
    if statusIsSuccessful (responseStatus response)
        then case AE.eitherDecode (responseBody response) of
            Left e ->
                throwIO . userError $
                    "Could not decode response when refreshing access token: " ++ e
            Right resp -> do
                return (Right (widgetUrl resp))
        else
            return (Left (responseStatus response))

-- | An error indicating why creating a Transak widget URL failed.
data WidgetUrlError
    = -- | Getting a widget URL from Transak failed.
      -- The 'String' describes the failure reason, but should be logged rather than forwarded
      -- to the client.
      WUEBadGateway !String
    | -- | Getting the URL timed-out.
      WUEGatewayTimeout
    deriving (Show)

-- | Try to create a Transak widget URL for the given account to purchase CCD.
--  This will refresh the access token if necessary before trying to get the URL.
--  If getting the URL fails due to an invalid access token (which could happen due to a concurrent call)
--  then the process is retried (but only once).
--  If the URL could not be generated within 10 seconds, this will fail, signalling
--  'WUEGatewayTimeout'.
--  If an unexpected error occurred with the upstream server, including if the result could not be
--  decoded, 'WUEBadGateway' is returned, with a description of the cause.
tryCreateWidgetUrl :: TransakConfig -> MVar AccessToken -> AccountAddress -> IO (Either WidgetUrlError URI)
tryCreateWidgetUrl conf mvToken addr = do
    res <- race doIt (threadDelay 10000000)
    return $! case res of
        Left (Left err) -> Left $ WUEBadGateway err
        Left (Right uri) -> Right uri
        Right () -> Left $ WUEGatewayTimeout
  where
    doIt = do
        res <- try $ do
            token <- getAccessToken conf mvToken
            createWidgetUrl conf (accessToken token) addr >>= \case
                Left status
                    | status == status401 -> do
                        -- In the event of a 401, we retry in case another thread concurrently
                        -- refreshed the token, causing createWidgetUrl to fail.
                        token' <- getAccessToken conf mvToken
                        createWidgetUrl conf (accessToken token') addr >>= \case
                            Left e -> return (Left $ "(On retry) createWidgetUrl received status: " ++ show e)
                            Right uri -> return (Right uri)
                    | otherwise -> return (Left $ "createWidgetUrl received status: " ++ show status)
                Right uri -> return (Right uri)
        return $
            case res of
                Left (e :: IOException) -> Left $ "Failed creating widget URL: " ++ show e
                Right r -> r
