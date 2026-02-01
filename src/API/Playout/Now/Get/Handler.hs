-- | Handler for GET /api/playout/now.
module API.Playout.Now.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayoutResponse (..))
import App.Config (Environment (..))
import App.Domains (siteBaseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/now.
--
-- Returns the audio URL for the currently airing episode based on the schedule.
-- Returns null (PlayoutUnavailable) if no episode is currently scheduled,
-- if the scheduled episode has no audio uploaded, or on any database error.
-- Graceful degradation: any error returns null rather than failing.
handler :: AppM PlayoutResponse
handler = do
  currentTime <- liftIO getCurrentTime
  result <- execQuery $ Episodes.getCurrentlyAiringEpisode currentTime

  case result of
    Left _err -> pure PlayoutUnavailable -- Graceful degradation on DB error
    Right Nothing -> pure PlayoutUnavailable
    Right (Just episode) -> case episode.audioFilePath of
      Nothing -> pure PlayoutUnavailable
      Just audioPath -> do
        storageBackend <- asks (Has.getter @StorageBackend)
        env <- asks (Has.getter @Environment)
        let fullUrl = buildFullMediaUrl env storageBackend audioPath
        pure $ PlayoutAvailable fullUrl

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Environment -> StorageBackend -> Text -> Text
buildFullMediaUrl env backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> siteBaseUrl env <> buildMediaUrl backend objectKey
