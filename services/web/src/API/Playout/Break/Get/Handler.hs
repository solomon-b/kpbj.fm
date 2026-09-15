-- | Handler for GET /api/playout/break.
module API.Playout.Break.Get.Handler
  ( handler,

    -- * Exported for testing
    handlerAt,
    breakWindowSeconds,
    assumedStationIdSeconds,
    nextBoundary,
    fillWindow,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (BreakResponse, PlayoutTrack (..), sanitizeAnnotateValue)
import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime (..), getCurrentTime)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, secondsToDiffTime)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.StationIds qualified as StationIds
import Log qualified

--------------------------------------------------------------------------------

-- | How long a break window runs, in seconds.
--
-- Hosts are asked to deliver show audio this far short of their slot, so show
-- audio ends where the break begins.
breakWindowSeconds :: Int64
breakWindowSeconds = 120

-- | The length assumed for a station ID that carries no recorded duration.
--
-- Every station ID uploaded since the break window existed records its own
-- length. Older rows do not, and re-reading the audio here would mean fetching
-- the file on every break. Assuming a length keeps those rows usable; the cost
-- of guessing low is that a break can overrun by a few seconds, which the
-- boundary poll then cuts.
assumedStationIdSeconds :: Int64
assumedStationIdSeconds = 15

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/break.
--
-- Returns the tracks for one break window: a random station ID first, then the
-- PSAs and advertisement spots that fit in the rest of the window.
--
-- Returns an empty array when no break is due at the next boundary, and on any
-- database error. Liquidsoap treats an empty array as "play on", so a failure
-- here leaves the current show or filler untouched rather than cutting it for
-- silence.
handler :: AppM BreakResponse
handler = liftIO getCurrentTime >>= handlerAt

-- | 'handler', with the clock supplied.
--
-- The answer depends on where the instant falls against the schedule, so tests
-- pass a fixed one rather than racing the wall clock.
handlerAt :: UTCTime -> AppM BreakResponse
handlerAt currentTime = do
  let breakEnd = nextBoundary currentTime

  dueResult <- execQuery (ShowSchedule.isBreakDue breakEnd)
  case dueResult of
    Left err -> do
      Log.logAttention "Break window: schedule lookup failed" (show err)
      pure []
    Right False -> pure []
    Right True -> do
      storageBackend <- asks (Has.getter @StorageBackend)
      appBaseUrl <- baseUrl

      -- A station ID always opens the window. Its length comes off the budget
      -- before any break item is considered.
      stationIdResult <- execQuery StationIds.getRandomStationId
      let mStationId = case stationIdResult of
            Right (Just sid) -> Just sid
            _ -> Nothing
          stationIdLength =
            maybe 0 (fromMaybe assumedStationIdSeconds . (.simDurationSeconds)) mStationId
          budget = breakWindowSeconds - stationIdLength
          mStationIdTrack =
            fmap
              ( \sid ->
                  PlayoutTrack
                    { ptUrl = buildFullMediaUrl appBaseUrl storageBackend sid.simAudioFilePath,
                      ptTitle = sanitizeAnnotateValue sid.simTitle,
                      ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
                      ptSourceType = "station_id"
                    }
              )
              mStationId

      itemsResult <- execQuery (BreakItems.getEligibleForBreak (pacificDay breakEnd) budget)
      chosen <- case itemsResult of
        Left err -> do
          Log.logAttention "Break window: break item lookup failed" (show err)
          pure []
        Right candidates -> do
          let picked = fillWindow budget candidates
          case NonEmpty.nonEmpty (map (.bimId) picked) of
            Nothing -> pure ()
            Just ids -> do
              markResult <- execQuery (BreakItems.markPlayed ids)
              case markResult of
                -- The rotation stalls for one window rather than the break
                -- failing. The items still air.
                Left err -> Log.logAttention "Break window: could not stamp last_played_at" (show err)
                Right () -> pure ()
          pure picked

      let itemTracks =
            map
              ( \item ->
                  PlayoutTrack
                    { ptUrl = buildFullMediaUrl appBaseUrl storageBackend item.bimAudioFilePath,
                      ptTitle = sanitizeAnnotateValue item.bimTitle,
                      ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
                      ptSourceType = "break_item"
                    }
              )
              chosen

      pure $ maybe itemTracks (: itemTracks) mStationIdTrack

--------------------------------------------------------------------------------

-- | The next half-hour boundary at or after this instant.
--
-- Liquidsoap asks at @:28@ and @:58@, so this lands on the @:30@ or the @:00@
-- two minutes later. The result carries whole seconds, because
-- 'ShowSchedule.isBreakDue' compares it to a slot end for equality and a slot
-- end is a date plus a @TIME@.
--
-- An instant already exactly on a boundary maps to itself. Any fraction of a
-- second past one moves to the next, so the answer is never behind the caller.
nextBoundary :: UTCTime -> UTCTime
nextBoundary (UTCTime day dayTime) =
  let halfHour = 1800 :: Integer
      picosPerSecond = 1000000000000 :: Integer
      -- Both of these round up. The time of day is never negative, so the
      -- (a + b - 1) `div` b form is safe.
      seconds = (diffTimeToPicoseconds dayTime + picosPerSecond - 1) `div` picosPerSecond
      rounded = ((seconds + halfHour - 1) `div` halfHour) * halfHour
   in -- A boundary past the end of the day rolls into the next one. 86400 is a
      -- multiple of 1800, so this only happens on a leap second day.
      if rounded >= 86400
        then UTCTime (succ day) 0
        else UTCTime day (secondsToDiffTime rounded :: DiffTime)

-- | Take items from the front of the list while they fit in the budget.
--
-- The list arrives ordered by priority then by least recently played, so
-- walking from the front spends the window on the items that most deserve it.
--
-- An item that does not fit is skipped rather than ending the walk, so one long
-- spot near the front cannot waste the rest of the window. Every item in the
-- list already fits the budget on its own; the query drops the ones that do
-- not.
fillWindow :: Int64 -> [BreakItems.Model] -> [BreakItems.Model]
fillWindow budget = go 0
  where
    go _ [] = []
    go used (item : rest)
      | used + item.bimDurationSeconds <= budget =
          item : go (used + item.bimDurationSeconds) rest
      | otherwise = go used rest

--------------------------------------------------------------------------------

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Text -> StorageBackend -> Text -> Text
buildFullMediaUrl appBaseUrl backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> appBaseUrl <> buildMediaUrl backend objectKey
