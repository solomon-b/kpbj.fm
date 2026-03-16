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
import Effects.Database.Tables.StationIds qualified as StationIds

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/fallback.
--
-- Returns a JSON array of tracks for fallback playback:
-- 1. A randomly selected station ID (if any exist)
-- 2. A randomly selected ephemeral upload
--
-- Returns an empty array if no ephemeral uploads exist or on ephemeral DB error.
-- Station ID DB errors are silently skipped (only the ephemeral track is returned).
handler :: AppM FallbackResponse
handler = do
  ephemeralResult <- execQuery EphemeralUploads.getRandomEphemeralUpload

  case ephemeralResult of
    Left _err -> pure [] -- Graceful degradation on DB error
    Right Nothing -> pure []
    Right (Just upload) -> do
      storageBackend <- asks (Has.getter @StorageBackend)
      appBaseUrl <- baseUrl

      let ephemeralTrack =
            PlayoutTrack
              { ptUrl = buildFullMediaUrl appBaseUrl storageBackend upload.eumAudioFilePath,
                ptTitle = sanitizeAnnotateValue upload.eumTitle,
                ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
                ptSourceType = "ephemeral"
              }

      -- Try to get a station ID; skip on error or if none exist
      stationIdResult <- execQuery StationIds.getRandomStationId
      let mStationIdTrack = case stationIdResult of
            Right (Just sid) ->
              Just
                PlayoutTrack
                  { ptUrl = buildFullMediaUrl appBaseUrl storageBackend sid.simAudioFilePath,
                    ptTitle = sanitizeAnnotateValue sid.simTitle,
                    ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
                    ptSourceType = "station_id"
                  }
            _ -> Nothing

      pure $ maybe [ephemeralTrack] (\sidTrack -> [sidTrack, ephemeralTrack]) mStationIdTrack

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Text -> StorageBackend -> Text -> Text
buildFullMediaUrl appBaseUrl backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> appBaseUrl <> buildMediaUrl backend objectKey
