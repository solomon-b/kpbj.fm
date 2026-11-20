module API.Dashboard.StreamSettings.Restart.Icecast.Post.Handler (handler, action) where

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

handler ::
  Satisfies 'Admin RoleAuth ->
  AppM (Lucid.Html ())
handler _ =
  handleBannerErrors "Restart Icecast" $ do
    result <- action
    pure $ case result of
      WebhookSuccess -> renderBanner Success "Icecast Restarting" "The Icecast container is restarting. Stream will be back shortly."
      WebhookNotConfigured -> renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot restart containers."
      WebhookError _ -> renderBanner Error "Restart Failed" "Failed to restart Icecast. Check server logs for details."

--------------------------------------------------------------------------------

-- | Business logic: call restart-icecast webhook.
action :: ExceptT HandlerError AppM WebhookResult
action = do
  webhookConfig <- asks (Has.getter @WebhookConfig)
  result <- lift $ callWebhook webhookConfig "restart-icecast"
  case result of
    WebhookSuccess -> Log.logInfo "Icecast restart triggered" Aeson.Null
    WebhookNotConfigured -> Log.logInfo "Webhook not configured for icecast restart" Aeson.Null
    WebhookError errMsg -> Log.logInfo "Icecast restart failed" (Aeson.object ["error" .= errMsg])
  pure result
