module API.Dashboard.StreamSettings.StartStream.Post.Handler (handler) where

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

-- | Start the entire stream by starting Icecast then Liquidsoap.
handler ::
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler cookie =
  handleBannerErrors "Start Stream" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can start the stream." userMetadata

    webhookConfig <- asks (Has.getter @WebhookConfig)
    result <- callWebhook webhookConfig "start-stream"

    case result of
      WebhookSuccess -> do
        Log.logInfo "Stream start triggered" Aeson.Null
        pure $ renderBanner Success "Stream Starting" "Icecast and Liquidsoap are starting up."
      WebhookNotConfigured -> do
        Log.logAttention "Webhook not configured for stream start" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot start stream."
      WebhookError errMsg -> do
        Log.logAttention "Stream start failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Start Failed" "Failed to start the stream. Check server logs for details."
