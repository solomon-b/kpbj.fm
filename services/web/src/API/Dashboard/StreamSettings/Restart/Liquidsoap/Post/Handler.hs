module API.Dashboard.StreamSettings.Restart.Liquidsoap.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import App.CustomContext (WebhookConfig)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
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
    result <- action
    pure $ case result of
      WebhookSuccess -> renderBanner Success "Liquidsoap Restarting" "The Liquidsoap container is restarting. Audio playback will resume shortly."
      WebhookNotConfigured -> renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot restart containers."
      WebhookError _ -> renderBanner Error "Restart Failed" "Failed to restart Liquidsoap. Check server logs for details."

--------------------------------------------------------------------------------

-- | Business logic: call restart-liquidsoap webhook.
action :: ExceptT HandlerError AppM WebhookResult
action = do
  webhookConfig <- asks (Has.getter @WebhookConfig)
  result <- lift $ callWebhook webhookConfig "restart-liquidsoap"
  case result of
    WebhookSuccess -> Log.logInfo "Liquidsoap restart triggered" Aeson.Null
    WebhookNotConfigured -> Log.logInfo "Webhook not configured for liquidsoap restart" Aeson.Null
    WebhookError errMsg -> Log.logInfo "Liquidsoap restart failed" (Aeson.object ["error" .= errMsg])
  pure result
