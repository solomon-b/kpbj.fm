-- | Handler for GET /api/playout/now.
module API.Playout.Now.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (NowPlayingResponse (..), mkPlayoutMetadata)
import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Log qualified

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/now.
--
-- Returns the audio URL for the currently airing episode based on the schedule.
-- Returns null (NothingPlaying) if no episode is currently scheduled,
-- if the scheduled episode has no audio uploaded, or on any database error.
-- Graceful degradation: any error returns null rather than failing.
handler :: AppM NowPlayingResponse
handler = do
  currentTime <- liftIO getCurrentTime
  result <- execQuery $ Episodes.getCurrentlyAiringEpisodes currentTime

  mEpisode <- case result of
    Left _err -> pure Nothing -- Graceful degradation on DB error
    Right [] -> pure Nothing
    Right (episode : rest) -> do
      -- Two shows claim this time. The order is stable, so the stream stays on
      -- one of them, but the overlap is a data defect and it silences the other.
      unless (null rest) $
        Log.logAttention
          "More than one episode is airing now"
          (show (map (.id) (episode : rest)))
      pure (Just episode)

  case mEpisode of
    Nothing -> pure NothingPlaying
    Just episode -> case episode.audioFilePath of
      Nothing -> pure NothingPlaying
      Just audioPath -> do
        -- Fetch show info for metadata
        showResult <- execQuery $ Shows.getShowById episode.showId
        let showTitle = maybe "KPBJ 95.9 FM" (.title) (fromRight Nothing showResult)
            metadata = mkPlayoutMetadata showTitle "KPBJ 95.9 FM"

        storageBackend <- asks (Has.getter @StorageBackend)
        appBaseUrl <- baseUrl
        let fullUrl = buildFullMediaUrl appBaseUrl storageBackend audioPath
        pure $ NowPlaying fullUrl metadata

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Text -> StorageBackend -> Text -> Text
buildFullMediaUrl appBaseUrl backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> appBaseUrl <> buildMediaUrl backend objectKey
