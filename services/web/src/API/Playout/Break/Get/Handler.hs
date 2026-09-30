-- | Handler for GET /api/playout/break.
module API.Playout.Break.Get.Handler
  ( handler,
    action,

    -- * Exported for testing
    nextBoundary,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (BreakResponse, PlayoutTrack (..), sanitizeAnnotateValue)
import App.BaseUrl (baseUrl)
import App.Handler.Combinators (requirePlayoutSecret)
import App.Monad (AppM)
import App.Storage (StorageBackend (..), buildMediaUrl)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (runExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, secondsToDiffTime)
import Domain.Types.Timezone (pacificDay)
import Effects.BreakPlan qualified as BreakPlan
import Effects.Clock (currentSystemTime)
import Effects.Database.Tables.BreakPlans (PlannedTrack (..))
import Log qualified
import Servant.Server (err401)

--------------------------------------------------------------------------------

-- | Handler for GET /api/playout/break.
--
-- Returns the planned tracks of the break before the next boundary: a station
-- ID first, then underwriting announcements in a show break, then PSAs.
--
-- Returns an empty array when the boundary has no break, and on any database
-- error. Liquidsoap treats an empty array as "play on", so a failure here
-- leaves the current show or filler untouched rather than cutting it for
-- silence.
--
-- A bad or absent @X-Playout-Secret@ header answers 401 rather than an empty
-- array. An empty array reads as "no break is due", which would hide a
-- misconfigured secret until someone noticed the breaks had stopped. Liquidsoap
-- logs the status and leaves the current source alone either way.
handler :: Maybe Text -> AppM BreakResponse
handler mSecret =
  runExceptT (requirePlayoutSecret mSecret) >>= \case
    Left _ -> throwM err401
    Right () -> currentSystemTime >>= action

-- | The planned tracks for the break before the next boundary.
--
-- Builds today's plan if it does not exist, then reads the stored rows for this
-- boundary. It chooses nothing, so a repeated request returns the same tracks.
action :: UTCTime -> AppM BreakResponse
action currentTime = do
  let breakEnd = nextBoundary currentTime
      -- A break belongs to the Pacific day it starts on. The 23:58 break ends at
      -- 00:00 the next day, but it is the last break of the day it starts on.
      breakStart = addUTCTime (-120) breakEnd
  built <- BreakPlan.ensurePlan (pacificDay breakStart)
  case built of
    Left err -> do
      Log.logAttention "Break window: could not build the plan" (show err)
      pure []
    Right () -> do
      result <- BreakPlan.tracksForBoundary breakEnd
      case result of
        Left err -> do
          Log.logAttention "Break window: could not read the plan" (show err)
          pure []
        Right tracks -> do
          storageBackend <- asks (Has.getter @StorageBackend)
          appBaseUrl <- baseUrl
          pure
            [ PlayoutTrack
                { ptUrl = buildFullMediaUrl appBaseUrl storageBackend t.plAudioFilePath,
                  ptTitle = sanitizeAnnotateValue t.plTitle,
                  ptArtist = sanitizeAnnotateValue "KPBJ 95.9 FM",
                  ptSourceType = t.plSourceType,
                  ptBreakItemId = t.plBreakItemId
                }
            | t <- tracks
            ]

--------------------------------------------------------------------------------

-- | The next half-hour boundary at or after this instant.
--
-- Liquidsoap asks at @:28@ and @:58@, so this lands on the @:30@ or the @:00@
-- two minutes later. The result carries whole seconds, because the plan is
-- keyed by boundary and every stored boundary is a whole half-hour.
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

-- | Build a full URL for media files, ensuring external services can fetch them.
--
-- For S3 storage, buildMediaUrl already returns a full URL.
-- For local storage, we prepend the site base URL.
buildFullMediaUrl :: Text -> StorageBackend -> Text -> Text
buildFullMediaUrl appBaseUrl backend objectKey = case backend of
  S3Storage _ -> buildMediaUrl backend objectKey
  LocalStorage _ -> appBaseUrl <> buildMediaUrl backend objectKey
