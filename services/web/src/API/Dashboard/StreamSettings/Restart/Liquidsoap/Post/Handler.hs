module API.Dashboard.StreamSettings.Restart.Liquidsoap.Post.Handler (handler) where

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler cookie =
  handleBannerErrors "Restart Liquidsoap" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can restart containers." userMetadata

    webhookConfig <- asks (Has.getter @WebhookConfig)
    result <- callWebhook webhookConfig "restart-liquidsoap"

    case result of
      WebhookSuccess -> do
        Log.logInfo "Liquidsoap restart triggered" Aeson.Null
        pure $ renderBanner Success "Liquidsoap Restarting" "The Liquidsoap container is restarting. Audio playback will resume shortly."
      WebhookNotConfigured -> do
        Log.logInfo "Webhook not configured for liquidsoap restart" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot restart containers."
      WebhookError errMsg -> do
        Log.logInfo "Liquidsoap restart failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Restart Failed" "Failed to restart Liquidsoap. Check server logs for details."
