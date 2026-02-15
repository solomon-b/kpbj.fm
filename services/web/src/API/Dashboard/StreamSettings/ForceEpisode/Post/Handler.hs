{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StreamSettings.ForceEpisode.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StreamSettings.ForceEpisode.Post.Route (ForceEpisodeForm (..))
import App.Config (Environment (..))
import App.CustomContext (WebhookConfig)
import App.Domains (siteBaseUrl)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors, throwNotFound)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Webhook (WebhookResult (..), callWebhookWithPayload)
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/stream-settings/force-episode.
--
-- Validates the episode exists and has audio, resolves the full audio URL,
-- then pushes it to liquidsoap via the webhook service.
handler ::
  Maybe Cookie ->
  ForceEpisodeForm ->
  AppM (Lucid.Html ())
handler cookie ForceEpisodeForm {..} =
  handleBannerErrors "Force episode" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can force-play episodes." userMetadata

    let epId = Episodes.Id episodeId

    -- 1. Fetch episode
    episodeResult <- execQuery (Episodes.getEpisodeById epId)
    episode <- case episodeResult of
      Left _err -> throwNotFound "Episode"
      Right Nothing -> throwNotFound "Episode"
      Right (Just ep) -> pure ep

    -- 2. Validate audio exists
    audioPath <- case episode.audioFilePath of
      Nothing -> throwNotFound "Episode audio"
      Just path -> pure path

    -- 3. Fetch show title (fallback to station name on error)
    showResult <- execQuery $ Shows.getShowById episode.showId
    showTitle <- case showResult of
      Left err -> do
        Log.logInfo "Failed to fetch show for force-play" (Aeson.object ["error" .= show err, "showId" .= episode.showId])
        pure "KPBJ 95.9 FM"
      Right Nothing -> pure "KPBJ 95.9 FM"
      Right (Just s) -> pure s.title

    -- 4. Build full audio URL
    storageBackend <- asks (Has.getter @StorageBackend)
    env <- asks (Has.getter @Environment)
    let fullUrl = buildFullMediaUrl env storageBackend audioPath

    -- 5. Call webhook (sanitize metadata to avoid breaking liquidsoap annotate URI)
    webhookConfig <- asks (Has.getter @WebhookConfig)
    let payload =
          Aeson.object
            [ "url" .= fullUrl,
              "title" .= sanitizeAnnotateValue showTitle,
              "artist" .= sanitizeAnnotateValue ("KPBJ 95.9 FM" :: Text)
            ]
    result <- callWebhookWithPayload webhookConfig "force-play-episode" payload

    case result of
      WebhookSuccess -> do
        let epNum = episode.episodeNumber
        Log.logInfo "Force-play episode triggered" (Aeson.object ["episodeId" .= episodeId, "showTitle" .= showTitle, "url" .= fullUrl])
        pure $ renderBanner Success "Force Play Sent" [i|Now playing: #{showTitle} Ep. #{epNum}|]
      WebhookNotConfigured -> do
        Log.logInfo "Webhook not configured for force-play" Aeson.Null
        pure $ renderBanner Error "Not Configured" "Webhook URL or secret is not configured. Cannot force-play episodes."
      WebhookError errMsg -> do
        Log.logInfo "Force-play episode failed" (Aeson.object ["error" .= errMsg])
        pure $ renderBanner Error "Force Play Failed" "Failed to force-play episode. Check server logs for details."

-- | Sanitize a text value for use in a liquidsoap @annotate:@ URI.
--
-- The annotate format uses double quotes as value delimiters and newlines
-- as command separators in the telnet protocol. Characters that would break
-- the format are stripped to prevent malformed commands.
sanitizeAnnotateValue :: Text -> Text
sanitizeAnnotateValue = Text.filter (`notElem` ['"', '\n', '\r', '\\'])

-- | Build a full URL for media files, ensuring external services can fetch them.
buildFullMediaUrl :: Environment -> StorageBackend -> Text -> Text
buildFullMediaUrl env backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> siteBaseUrl env <> buildMediaUrl backend objectKey
