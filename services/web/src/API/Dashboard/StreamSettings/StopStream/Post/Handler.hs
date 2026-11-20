module API.Dashboard.StreamSettings.StopStream.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.CustomContext (WebhookConfig)
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Effects.Webhook (WebhookResult (..), callWebhook)
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Stop the entire stream by stopping Liquidsoap then Icecast.
handler ::
  Satisfies 'Admin RoleAuth ->
  AppM (Lucid.Html ())
handler _ =
  handleBannerErrors "Stop Stream" $ do
    result <- action
    pure $ case result of
      WebhookSuccess -> renderBanner Success "Stream Stopping" "Liquidsoap and Icecast are shutting down."
      WebhookNotConfigured -> renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot stop stream."
      WebhookError _ -> renderBanner Error "Stop Failed" "Failed to stop the stream. Check server logs for details."

--------------------------------------------------------------------------------

-- | Business logic: call stop-stream webhook.
action :: ExceptT HandlerError AppM WebhookResult
action = do
  webhookConfig <- asks (Has.getter @WebhookConfig)
  result <- lift $ callWebhook webhookConfig "stop-stream"
  case result of
    WebhookSuccess -> Log.logInfo "Stream stop triggered" Aeson.Null
    WebhookNotConfigured -> Log.logAttention "Webhook not configured for stream stop" Aeson.Null
    WebhookError errMsg -> Log.logAttention "Stream stop failed" (Aeson.object ["error" .= errMsg])
  pure result
