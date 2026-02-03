-- | Handler for GET /api/playout/fallback.
module API.Playout.Fallback.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayoutMetadata (..), PlayoutResponse (..))
import App.Config (Environment (..))
import App.Domains (siteBaseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/fallback.
--
-- Returns the audio URL for a randomly selected ephemeral upload.
-- Returns null (PlayoutUnavailable) if no ephemeral uploads exist or on any database error.
-- Used by Liquidsoap for fallback audio when no show is scheduled.
-- Graceful degradation: any error returns null rather than failing.
handler :: AppM PlayoutResponse
handler = do
  result <- execQuery EphemeralUploads.getRandomEphemeralUpload

  case result of
    Left _err -> pure PlayoutUnavailable -- Graceful degradation on DB error
    Right Nothing -> pure PlayoutUnavailable
    Right (Just upload) -> do
      let metadata = PlayoutMetadata {title = upload.eumTitle, artist = "KPBJ 95.9 FM"}
      storageBackend <- asks (Has.getter @StorageBackend)
      env <- asks (Has.getter @Environment)
      let fullUrl = buildFullMediaUrl env storageBackend upload.eumAudioFilePath
      pure $ PlayoutAvailable fullUrl metadata

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Environment -> StorageBackend -> Text -> Text
buildFullMediaUrl env backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> siteBaseUrl env <> buildMediaUrl backend objectKey
