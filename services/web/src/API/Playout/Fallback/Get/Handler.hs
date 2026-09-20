-- | Handler for GET /api/playout/fallback.
module API.Playout.Fallback.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (FallbackResponse, PlayoutTrack (..), sanitizeAnnotateValue)
import App.BaseUrl (baseUrl)
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
-- Returns a one-element array holding a randomly selected ephemeral upload.
--
-- Returns an empty array if no ephemeral uploads exist or on a database error.
-- Liquidsoap then asks again rather than cutting to silence.
--
-- A station ID used to lead every ephemeral track. Break windows place station
-- IDs now, so the filler pool no longer does. The fallback only plays when no
-- break and no show is on air, and a break opens every hour that no slot spans,
-- so the hourly identification still lands.
handler :: AppM FallbackResponse
handler = do
  ephemeralResult <- execQuery EphemeralUploads.getRandomEphemeralUpload

  case ephemeralResult of
    Left _err -> pure [] -- Graceful degradation on DB error
    Right Nothing -> pure []
    Right (Just upload) -> do
      storageBackend <- asks (Has.getter @StorageBackend)
      appBaseUrl <- baseUrl

      pure
        [ PlayoutTrack
            { ptUrl = buildFullMediaUrl appBaseUrl storageBackend upload.eumAudioFilePath,
              ptTitle = sanitizeAnnotateValue upload.eumTitle,
              ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
              ptSourceType = "ephemeral"
            }
        ]

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Text -> StorageBackend -> Text -> Text
buildFullMediaUrl appBaseUrl backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> appBaseUrl <> buildMediaUrl backend objectKey
