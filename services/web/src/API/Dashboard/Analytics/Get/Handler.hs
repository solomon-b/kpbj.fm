{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Analytics.Get.Handler
  ( handler,

    -- * Exported for testing
    rangeToParams,
    roundTo1,
    mkTopEpisode,
    mkTopDimension,
    mkTopShow,
    mkHeatmap,
    mkNewsletter,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Templates.Page qualified as Templates
import API.Dashboard.Analytics.Get.Types
import API.Links (apiLinks, showEpisodesLinks, showsLinks)
import API.Types (Routes (..), ShowEpisodesRoutes (..), ShowsRoutes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.CustomContext (StreamConfig (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, showGregorian)
import Domain.Icecast.Status (parseListenerCount)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificDay, startOfPacificDay, utcToPacific)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.GaSnapshots qualified as GaSnapshots
import Effects.Database.Tables.ListenerSnapshots qualified as ListenerSnapshots
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Satisfies 'UserMetadata.Admin RoleAuth ->
  Maybe AnalyticsRange ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (fromMaybe defaultRange -> range) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Analytics" apiLinks.rootGet $ do
    vd <- action auth.raMetadata range
    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        vd.avUserMetadata
        NoShow
        NavAnalytics
        Nothing
        Nothing
        (Templates.template vd.avRange vd.avCurrentListeners vd.avData)

--------------------------------------------------------------------------------

-- | All data needed to render the analytics page.
data AnalyticsViewData = AnalyticsViewData
  { avUserMetadata :: UserMetadata.Model,
    avRange :: AnalyticsRange,
    avCurrentListeners :: Maybe Int64,
    avData :: AnalyticsData
  }

-- | Business logic: fetch the current listener count from Icecast and the
-- analytics data for the range.
action :: UserMetadata.Model -> AnalyticsRange -> ExceptT HandlerError AppM AnalyticsViewData
action userMetadata range = do
  currentListeners <- lift fetchCurrentListeners
  analytics <- lift $ loadAnalyticsData range

  pure
    AnalyticsViewData
      { avUserMetadata = userMetadata,
        avRange = range,
        avCurrentListeners = currentListeners,
        avData = analytics
      }

--------------------------------------------------------------------------------

-- | Fetch the current listener count directly from Icecast.
fetchCurrentListeners :: AppM (Maybe Int64)
fetchCurrentListeners = do
  streamCfg <- asks (getter @StreamConfig)
  result <- liftIO $ try @HTTP.HttpException $ do
    request <- HTTP.parseRequest (Text.unpack streamCfg.scMetadataUrl)
    let request' = HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 5_000_000) request
    response <- HTTP.httpLBS request'
    pure (HTTP.getResponseBody response)
  case result of
    Left _ -> pure Nothing
    Right body -> pure (fromIntegral @Int32 @Int64 <$> parseListenerCount body)

--------------------------------------------------------------------------------

-- | Aggregate the listener, archive-play, and Google Analytics data for a
-- range.
loadAnalyticsData :: AnalyticsRange -> AppM AnalyticsData
loadAnalyticsData range = do
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

  -- Google Analytics rollups (referrers, countries, cities)
  refs <-
    queryOr [] "getTopReferrers" $
      execQuery (GaSnapshots.getTopReferrers start now topDimensionLimit)

  countries <-
    queryOr [] "getTopCountries" $
      execQuery (GaSnapshots.getTopCountries start now topDimensionLimit)

  cities <-
    queryOr [] "getTopCities" $
      execQuery (GaSnapshots.getTopCities start now topDimensionLimit)

  -- Listening by weekday and hour, top shows, and newsletter growth
  heatmapRows <-
    queryOr [] "getHourlyHeatmap" $
      execQuery (ListenerSnapshots.getHourlyHeatmap start now)

  showRows <-
    queryOr [] "getTopShows" $
      execQuery (EpisodePlayEvents.getTopShows start now topShowsLimit)

  subscribersBefore <-
    queryOr 0 "countCreatedBefore" $
      execQuery (NewsletterSubscribers.countCreatedBefore start)

  signups <-
    queryOr [] "getDailySignups" $
      execQuery (NewsletterSubscribers.getDailySignups start now)

  -- The newsletter line has one point for each day, so the all-time line
  -- starts on the day of the first signup.
  newsletterStart <- case range of
    RangeAll ->
      fromMaybe now
        <$> queryOr Nothing "getFirstCreatedAt" (execQuery NewsletterSubscribers.getFirstCreatedAt)
    _ -> pure start

  pure
    AnalyticsData
      { listeners =
          ListenerData
            { series =
                ChartSeries
                  { labels = map (formatPacific . snd) snapshots,
                    values = map fst snapshots
                  },
              peak = fromIntegral (fromMaybe 0 peak :: Int64),
              avg = roundTo1 (fromMaybe 0 avg)
            },
        archivePlays =
          ArchivePlayData
            { series =
                ChartSeries
                  { labels = map snd plays,
                    values = map (\(count, _) -> fromIntegral (count :: Int64)) plays
                  },
              total = fromIntegral (fromMaybe 0 total :: Int64)
            },
        topEpisodes = zipWith mkTopEpisode [1 ..] topEps,
        topReferrers = zipWith mkTopDimension [1 ..] refs,
        topCountries = zipWith mkTopDimension [1 ..] countries,
        topCities = zipWith mkTopDimension [1 ..] cities,
        listenerHeatmap = mkHeatmap heatmapRows,
        topShows = zipWith mkTopShow [1 ..] showRows,
        newsletter = mkNewsletter (pacificDay newsletterStart) (pacificDay now) subscribersBefore signups
      }

-- | Execute a query action, logging failures and returning a default value.
--
-- A failed query does not stop the page. The page shows the default value
-- for that query, and the log records the error.
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

-- | Number of rows to return per Google Analytics dimension.
topDimensionLimit :: Int32
topDimensionLimit = 10

-- | Number of top shows to return.
topShowsLimit :: Int32
topShowsLimit = 10

-- | Map a range to (start time, bucket size).
--
-- The 24h range starts 24 hours before now. A range of days starts at
-- midnight Pacific, and today is its last day, so the first day is a full
-- day.
rangeToParams :: AnalyticsRange -> UTCTime -> (UTCTime, ListenerSnapshots.BucketSize)
rangeToParams range now = case range of
  Range24h -> (addUTCTime (-(24 * 3_600)) now, ListenerSnapshots.FiveMinute)
  Range7d -> (daysBack 7, ListenerSnapshots.Hourly)
  Range30d -> (daysBack 30, ListenerSnapshots.Hourly)
  Range90d -> (daysBack 90, ListenerSnapshots.Daily)
  RangeAll -> (allTimeStart, ListenerSnapshots.Daily)
  where
    daysBack n = startOfPacificDay (addDays (1 - n) (pacificDay now))

-- | The lower bound of the all-time range. It is before every row, so the
-- queries get all the data.
allTimeStart :: UTCTime
allTimeStart = UTCTime (fromGregorian 1970 1 1) 0

-- | Format a UTCTime in station time for chart labels.
formatPacific :: UTCTime -> Text
formatPacific = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M" . utcToPacific

-- | Round a Double to one decimal place.
roundTo1 :: Double -> Double
roundTo1 x = fromIntegral (round (x * 10) :: Int) / 10

-- | Build a TopDimensionEntry from a ranked GA query row.
mkTopDimension :: Int -> GaSnapshots.TopDimensionRow -> TopDimensionEntry
mkTopDimension n row =
  TopDimensionEntry
    { rank = n,
      label = row.value,
      sessions = fromIntegral row.sessions
    }

-- | Build a TopShowEntry from a ranked query row.
mkTopShow :: Int -> EpisodePlayEvents.TopShowRow -> TopShowEntry
mkTopShow n row =
  let showUrl = Links.linkURI $ showsLinks.detail (Slug row.showSlug) Nothing
   in TopShowEntry
        { rank = n,
          title = row.showTitle,
          plays = fromIntegral row.playCount,
          url = [i|/#{showUrl}|]
        }

-- | Arrange the heatmap rows in a grid of 7 weekdays by 24 hours. A row
-- with a weekday or an hour out of range is left out.
mkHeatmap :: [ListenerSnapshots.HeatmapRow] -> Heatmap
mkHeatmap rows =
  Heatmap [[Map.lookup (weekday, hour) byCell | hour <- [0 .. 23]] | weekday <- [1 .. 7]]
  where
    byCell = Map.fromList [((row.weekday, row.hour), row.avgListeners) | row <- rows]

-- | Build the newsletter growth chart with one point for each day of the
-- range. Each point is the total at the end of that day.
mkNewsletter ::
  -- | The first day of the range
  Day ->
  -- | The last day of the range
  Day ->
  -- | The subscribers from before the range
  Int64 ->
  -- | The signups for each day with signups, as (count, @YYYY-MM-DD@)
  [(Int64, Text)] ->
  NewsletterData
mkNewsletter firstDay lastDay before signups =
  NewsletterData
    { series =
        ChartSeries
          { labels = dayLabels,
            values = map fromIntegral (drop 1 (scanl (+) before dailyCounts))
          },
      total = fromIntegral (before + sum dailyCounts),
      new = fromIntegral (sum dailyCounts)
    }
  where
    dayLabels = map (Text.pack . showGregorian) [firstDay .. lastDay]
    byDay = Map.fromList [(day, count) | (count, day) <- signups]
    dailyCounts = map (\day -> Map.findWithDefault 0 day byDay) dayLabels

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
