{-# LANGUAGE QuasiQuotes #-}

-- | Handler for GET /dashboard/analytics/data?range=...
--
-- Returns JSON-encoded analytics data consumed by the Alpine.js charts
-- on the analytics dashboard page.
module API.Dashboard.Analytics.Data.Get.Handler
  ( handler,

    -- * Exported for testing
    rangeToParams,
    roundTo1,
    mkTopEpisode,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Data.Get.Types
import API.Links (showEpisodesLinks)
import API.Types (ShowEpisodesRoutes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleJsonErrors)
import App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ListenerSnapshots qualified as ListenerSnapshots
import Log qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/analytics/data?range=...
--
-- Authenticates as admin, aggregates listener and archive-play data,
-- and returns structured JSON for the analytics dashboard charts.
handler ::
  Maybe Cookie ->
  Maybe Text ->
  AppM AnalyticsData
handler cookie mRange =
  handleJsonErrors "Analytics data" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can access analytics." userMetadata
    lift $ action (fromMaybe "7d" mRange)

action :: Text -> AppM AnalyticsData
action range = do
  now <- liftIO getCurrentTime
  let (start, bucket) = rangeToParams range now

  -- Listener data
  snapshots <-
    queryOr [] "getSnapshotBuckets" $
      execQuery (ListenerSnapshots.getSnapshotBuckets bucket start now)

  peak <-
    queryOr Nothing "getPeakInRange" $
      execQuery (ListenerSnapshots.getPeakInRange start now)

  avg <-
    queryOr Nothing "getAverageInRange" $
      execQuery (ListenerSnapshots.getAverageInRange start now)

  -- Archive play data
  plays <-
    queryOr [] "getDailyPlayCounts" $
      execQuery (EpisodePlayEvents.getDailyPlayCounts start now)

  total <-
    queryOr Nothing "getTotalPlays" $
      execQuery (EpisodePlayEvents.getTotalPlays start now)

  topEps <-
    queryOr [] "getTopEpisodes" $
      execQuery (EpisodePlayEvents.getTopEpisodes start now topEpisodesLimit)

  pure
    AnalyticsData
      { listeners =
          ListenerData
            { labels = map (formatUTC . snd) snapshots,
              listenerData = map fst snapshots,
              peak = fromIntegral (fromMaybe 0 peak :: Int64),
              avg = roundTo1 (fromMaybe 0 avg)
            },
        archivePlays =
          ArchivePlayData
            { labels = map snd plays,
              playData = map (\(count, _) -> fromIntegral (count :: Int64)) plays,
              total = fromIntegral (fromMaybe 0 total :: Int64)
            },
        topEpisodes = zipWith mkTopEpisode [1 ..] topEps
      }

-- | Execute a query action, logging failures and returning a default value.
--
-- Analytics queries degrade gracefully — a single DB failure shouldn't
-- break the entire dashboard, but it must be logged for debugging.
queryOr :: (Show e) => a -> Text -> AppM (Either e a) -> AppM a
queryOr def label m = do
  result <- m
  case result of
    Right val -> pure val
    Left err -> do
      Log.logAttention ("Analytics: " <> label <> " failed") (show err)
      pure def

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

-- | Build a TopEpisode from a ranked query row.
mkTopEpisode :: Int -> EpisodePlayEvents.TopEpisodeRow -> TopEpisode
mkTopEpisode n row =
  let episodeUrl =
        Links.linkURI $
          showEpisodesLinks.detail
            (Slug row.showSlug)
            (Episodes.EpisodeNumber (fromIntegral row.episodeNumber))
   in TopEpisode
        { rank = n,
          title = row.showTitle <> " #" <> Text.pack (show row.episodeNumber),
          showTitle = row.showTitle,
          plays = fromIntegral row.playCount,
          url = [i|/#{episodeUrl}|]
        }
