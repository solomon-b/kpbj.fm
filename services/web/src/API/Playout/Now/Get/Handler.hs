-- | Handler for GET /api/playout/now.
module API.Playout.Now.Get.Handler
  ( handler,

    -- * Exported for testing
    actionAt,
    offsetSeconds,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (NowPlayingResponse (..), mkPlayoutMetadata)
import App.BaseUrl (absoluteMediaUrl, baseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
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
-- the schedule, and the offset into its slot. Returns null (NothingPlaying) if
-- no episode is scheduled then, if the scheduled episode has no audio uploaded,
-- or on any database error.
-- Graceful degradation: any error returns null rather than failing.
handler :: AppM NowPlayingResponse
handler = currentSystemTime >>= actionAt

-- | 'handler' for a given instant.
--
-- The instant is the real time, not the look-ahead time. The offset must count
-- from the real time, or an episode would start 10 seconds ahead of the schedule.
actionAt :: UTCTime -> AppM NowPlayingResponse
actionAt now = do
  result <- execQuery $ Episodes.getAiringEpisodes (addUTCTime lookAhead now)

  mAiring <- case result of
    Left _err -> pure Nothing -- Graceful degradation on DB error
    Right [] -> pure Nothing
    Right (airing : rest) -> do
      -- More than one row claims this time. The order is stable, so the stream
      -- stays on one of them, but the extra rows are a data defect. Either two
      -- slots overlap, and the stream silences one of them, or one airing is
      -- counted twice. See 'Episodes.getAiringEpisodes'.
      unless (null rest) $
        Log.logAttention
          "More than one episode is airing now"
          (Aeson.object ["episode.ids" .= map ((.id) . (.aeEpisode)) (airing : rest)])
      pure (Just airing)

  case mAiring of
    Nothing -> pure NothingPlaying
    Just airing -> case airing.aeEpisode.audioFilePath of
      Nothing -> pure NothingPlaying
      Just audioPath -> do
        -- Fetch show info for metadata
        showResult <- execQuery $ Shows.getShowById airing.aeEpisode.showId
        let showTitle = maybe "KPBJ 95.9 FM" (.title) (fromRight Nothing showResult)
            metadata = mkPlayoutMetadata showTitle "KPBJ 95.9 FM"
            offset = offsetSeconds now airing.aeWindowStart

        storageBackend <- asks (Has.getter @StorageBackend)
        appBaseUrl <- baseUrl
        let fullUrl = absoluteMediaUrl appBaseUrl storageBackend audioPath
        pure $ NowPlaying fullUrl offset metadata

-- | The whole seconds from the start of a window to an instant.
--
-- The poll before a boundary runs before the window opens, so the difference can
-- be negative. That case gives 0, and the episode plays from the start.
offsetSeconds ::
  -- | The instant
  UTCTime ->
  -- | The start of the window
  UTCTime ->
  Int64
offsetSeconds now windowStart = max 0 (floor (diffUTCTime now windowStart))
