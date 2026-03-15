-- | Handler for GET /dashboard/analytics/data?range=...
--
-- Returns JSON-encoded analytics data consumed by the Alpine.js charts
-- on the analytics dashboard page.
module API.Dashboard.Analytics.Data.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Data.Get.Types
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError (..), logHandlerError)
import App.Monad (AppM)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (fromRight)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuery)
import Log qualified
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.ListenerSnapshots qualified as ListenerSnapshots
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/analytics/data?range=...
--
-- Authenticates as admin, aggregates listener and archive-play data,
-- and returns structured JSON for the analytics dashboard charts.
handler ::
  Maybe Cookie ->
  Maybe Text ->
  AppM AnalyticsData
handler cookie mRange = do
  authResult <- runExceptT $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can access analytics." userMetadata
  case authResult of
    Left err -> do
      logHandlerError "Analytics data" err
      throwM $ case err of
        NotAuthenticated -> Servant.err401
        NotAuthorized _ _ -> Servant.err403
        UserSuspended -> Servant.err403
        _ -> Servant.err500
    Right () -> action (fromMaybe "7d" mRange)

action :: Text -> AppM AnalyticsData
action range = do
  now <- liftIO getCurrentTime
  let (start, bucket) = rangeToParams range now

  -- Listener data
  snapshotsResult <- execQuery (ListenerSnapshots.getSnapshotBuckets bucket start now)
  let snapshots = fromRight [] snapshotsResult

  peakResult <- execQuery (ListenerSnapshots.getPeakInRange start now)
  let peak = fromMaybe 0 $ fromRight Nothing peakResult

  avgResult <- execQuery (ListenerSnapshots.getAverageInRange start now)
  let avg = fromMaybe 0 $ fromRight Nothing avgResult

  -- Archive play data
  playsResult <- execQuery (EpisodePlayEvents.getDailyPlayCounts start now)
  case playsResult of
    Left err -> Log.logAttention "getDailyPlayCounts failed" (show err)
    Right _ -> pure ()
  let plays = fromRight [] playsResult

  totalResult <- execQuery (EpisodePlayEvents.getTotalPlays start now)
  let total = fromMaybe 0 $ fromRight Nothing totalResult

  -- Top episodes
  topResult <- execQuery (EpisodePlayEvents.getTopEpisodes start now topEpisodesLimit)
  case topResult of
    Left err -> Log.logAttention "getTopEpisodes failed" (show err)
    Right _ -> pure ()
  let topEps = fromRight [] topResult

  pure
    AnalyticsData
      { listeners =
          ListenerData
            { labels = map (formatUTC . snd) snapshots,
              listenerData = map fst snapshots,
              peak = fromIntegral (peak :: Int64),
              avg = roundTo1 avg
            },
        archivePlays =
          ArchivePlayData
            { labels = map snd plays,
              playData = map (\(count, _) -> fromIntegral (count :: Int64)) plays,
              total = fromIntegral (total :: Int64)
            },
        topEpisodes = zipWith mkTopEpisode [1 ..] topEps
      }

-- | Number of top episodes to return.
topEpisodesLimit :: Int32
topEpisodesLimit = 10

-- | Map range string to (start time, bucket size).
rangeToParams :: Text -> UTCTime -> (UTCTime, ListenerSnapshots.BucketSize)
rangeToParams range now = case range of
  "24h" -> (addUTCTime (-(24 * 3600)) now, ListenerSnapshots.FiveMinute)
  "7d" -> (addUTCTime (-(7 * 86400)) now, ListenerSnapshots.Hourly)
  "30d" -> (addUTCTime (-(30 * 86400)) now, ListenerSnapshots.Hourly)
  "90d" -> (addUTCTime (-(90 * 86400)) now, ListenerSnapshots.Daily)
  _ -> (addUTCTime (-(7 * 86400)) now, ListenerSnapshots.Hourly)

-- | Format a UTCTime as ISO 8601 for chart labels.
formatUTC :: UTCTime -> Text
formatUTC = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Round a Double to one decimal place.
roundTo1 :: Double -> Double
roundTo1 x = fromIntegral (round (x * 10) :: Int) / 10

-- | Build a TopEpisode from a ranked tuple.
mkTopEpisode :: Int -> (Int64, Text, Int64, Int64) -> TopEpisode
mkTopEpisode n (_, showTitle, episodeNum, plays) =
  TopEpisode
    { rank = n,
      title = showTitle <> " #" <> Text.pack (show episodeNum),
      showTitle = showTitle,
      plays = fromIntegral plays
    }
