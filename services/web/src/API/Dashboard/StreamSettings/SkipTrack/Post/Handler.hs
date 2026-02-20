module API.Dashboard.StreamSettings.SkipTrack.Post.Handler (handler, action) where

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
  handleBannerErrors "Skip Track" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can skip tracks." userMetadata
    result <- action
    pure $ case result of
      WebhookSuccess -> renderBanner Success "Track Skipped" "Skipping to fallback audio."
      WebhookNotConfigured -> renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot skip track."
      WebhookError _ -> renderBanner Error "Skip Failed" "Failed to skip track. Check server logs for details."

--------------------------------------------------------------------------------

-- | Business logic: call skip-track webhook.
action :: ExceptT HandlerError AppM WebhookResult
action = do
  webhookConfig <- asks (Has.getter @WebhookConfig)
  result <- lift $ callWebhook webhookConfig "skip-track"
  case result of
    WebhookSuccess -> Log.logInfo "Skip track triggered" Aeson.Null
    WebhookNotConfigured -> Log.logAttention "Webhook not configured for skip track" Aeson.Null
    WebhookError errMsg -> Log.logAttention "Skip track failed" (Aeson.object ["error" .= errMsg])
  pure result
