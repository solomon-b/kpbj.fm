-- | Handler for GET /api/playout/now.
module API.Playout.Now.Get.Handler
  ( handler,

    -- * Exported for testing
    actionAt,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (NowPlayingResponse (..), mkPlayoutMetadata)
import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Log qualified

--------------------------------------------------------------------------------

-- | How far ahead @/now@ looks.
--
-- Liquidsoap polls at @:29:55@ and @:59:55@. Looking 10 seconds ahead finds the
-- show that starts at the boundary, so it downloads and buffers during the
-- break. In the middle of a slot it still finds the current show, so the poll at
-- startup works the same way.
lookAhead :: NominalDiffTime
lookAhead = 10

-- | Handler for GET /api/playout/now.
--
-- Returns the audio URL for the episode airing 10 seconds from now, based on
-- the schedule. Returns null (NothingPlaying) if no episode is scheduled then,
-- if the scheduled episode has no audio uploaded, or on any database error.
-- Graceful degradation: any error returns null rather than failing.
handler :: AppM NowPlayingResponse
handler = currentSystemTime >>= actionAt . addUTCTime lookAhead

-- | 'handler' for a given instant.
actionAt :: UTCTime -> AppM NowPlayingResponse
actionAt lookupTime = do
  result <- execQuery $ Episodes.getCurrentlyAiringEpisodes lookupTime

  mEpisode <- case result of
    Left _err -> pure Nothing -- Graceful degradation on DB error
    Right [] -> pure Nothing
    Right (episode : rest) -> do
      -- More than one row claims this time. The order is stable, so the stream
      -- stays on one of them, but the extra rows are a data defect. Either two
      -- slots overlap, and the stream silences one of them, or one airing is
      -- counted twice. See 'Episodes.getCurrentlyAiringEpisodes'.
      unless (null rest) $
        Log.logAttention
          "More than one episode is airing now"
          (Aeson.object ["episode.ids" .= map (.id) (episode : rest)])
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
