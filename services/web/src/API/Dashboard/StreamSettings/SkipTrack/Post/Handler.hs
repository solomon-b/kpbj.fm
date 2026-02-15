module API.Dashboard.StreamSettings.SkipTrack.Post.Handler (handler) where

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
  handleBannerErrors "Skip Track" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can skip tracks." userMetadata

    webhookConfig <- asks (Has.getter @WebhookConfig)
    result <- callWebhook webhookConfig "skip-track"

    case result of
      WebhookSuccess -> do
        Log.logInfo "Skip track triggered" Aeson.Null
        pure $ renderBanner Success "Track Skipped" "Skipping to fallback audio."
      WebhookNotConfigured -> do
        Log.logAttention "Webhook not configured for skip track" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot skip track."
      WebhookError errMsg -> do
        Log.logAttention "Skip track failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Skip Failed" "Failed to skip track. Check server logs for details."
