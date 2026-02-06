{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Webhook
  ( WebhookResult (..),
    callWebhook,
  )
where

--------------------------------------------------------------------------------

import App.CustomContext (WebhookConfig (..), WebhookSecret (..))
import App.Monad (AppM)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP

--------------------------------------------------------------------------------

-- | Result of calling the webhook service.
data WebhookResult
  = WebhookSuccess
  | WebhookNotConfigured
  | WebhookError Text

-- | Call the webhook service to execute a hook.
callWebhook :: WebhookConfig -> Text -> AppM WebhookResult
callWebhook WebhookDisabled _ = pure WebhookNotConfigured
callWebhook WebhookEnabled {..} hookId = do
  let url = Text.unpack [i|#{wcBaseUrl}/hooks/#{hookId}|]
  result <- liftIO $ try @HTTP.HttpException $ do
    request <- HTTP.parseRequest url
    let requestWithMethod =
          HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
            HTTP.setRequestMethod "POST" $
              HTTP.setRequestBodyJSON (Aeson.object []) $
                HTTP.addRequestHeader "X-Webhook-Secret" (Text.encodeUtf8 $ unWebhookSecret wcSecret) request
    HTTP.httpBS requestWithMethod

  case result of
    Left err -> pure $ WebhookError (Text.pack $ show err)
    Right response ->
      let status = HTTP.getResponseStatus response
       in if HTTP.statusIsSuccessful status
            then pure WebhookSuccess
            else pure $ WebhookError [i|HTTP #{HTTP.statusCode status}: #{Text.decodeUtf8Lenient $ HTTP.statusMessage status}|]
