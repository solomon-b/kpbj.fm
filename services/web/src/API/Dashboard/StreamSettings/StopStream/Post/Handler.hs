module API.Dashboard.StreamSettings.StopStream.Post.Handler (handler) where

import App.CustomContext (WebhookConfig)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Domain.Types.Cookie (Cookie (..))
import Effects.Webhook (WebhookResult (..), callWebhook)
import Log qualified
import Lucid qualified

-- | Stop the entire stream by stopping Liquidsoap then Icecast.
handler ::
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler cookie =
  handleBannerErrors "Stop Stream" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can stop the stream." userMetadata

    webhookConfig <- asks (Has.getter @WebhookConfig)
    result <- callWebhook webhookConfig "stop-stream"

    case result of
      WebhookSuccess -> do
        Log.logInfo "Stream stop triggered" Aeson.Null
        pure $ renderBanner Success "Stream Stopping" "Liquidsoap and Icecast are shutting down."
      WebhookNotConfigured -> do
        Log.logAttention "Webhook not configured for stream stop" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot stop stream."
      WebhookError errMsg -> do
        Log.logAttention "Stream stop failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Stop Failed" "Failed to stop the stream. Check server logs for details."
