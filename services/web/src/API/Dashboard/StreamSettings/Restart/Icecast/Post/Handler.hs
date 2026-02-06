module API.Dashboard.StreamSettings.Restart.Icecast.Post.Handler (handler) where

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
  handleBannerErrors "Restart Icecast" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can restart containers." userMetadata

    webhookConfig <- asks (Has.getter @WebhookConfig)
    result <- callWebhook webhookConfig "restart-icecast"

    case result of
      WebhookSuccess -> do
        Log.logInfo "Icecast restart triggered" Aeson.Null
        pure $ renderBanner Success "Icecast Restarting" "The Icecast container is restarting. Stream will be back shortly."
      WebhookNotConfigured -> do
        Log.logInfo "Webhook not configured for icecast restart" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot restart containers."
      WebhookError errMsg -> do
        Log.logInfo "Icecast restart failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Restart Failed" "Failed to restart Icecast. Check server logs for details."
