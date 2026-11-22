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
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Templates.Page qualified as Templates
import API.Dashboard.Analytics.Get.Types
import API.Links (apiLinks, showEpisodesLinks)
import API.Types (Routes (..), ShowEpisodesRoutes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.CustomContext (StreamConfig (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Domain.Icecast.Status (parseListenerCount)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.GaSnapshots qualified as GaSnapshots
import Effects.Database.Tables.ListenerSnapshots qualified as ListenerSnapshots
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
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (fromMaybe defaultRange -> range) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Analytics" apiLinks.rootGet $ do
    vd <- action auth.raMetadata range
    lift $
      renderDashboardTemplate
        hxRequest
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

  pure
    AnalyticsData
      { listeners =
          ListenerData
            { series =
                ChartSeries
                  { labels = map (formatUTC . snd) snapshots,
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
        topCities = zipWith mkTopDimension [1 ..] cities
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

-- | Number of rows to return per Google Analytics dimension.
topDimensionLimit :: Int32
topDimensionLimit = 10

-- | Map a range to (start time, bucket size).
rangeToParams :: AnalyticsRange -> UTCTime -> (UTCTime, ListenerSnapshots.BucketSize)
rangeToParams range now = case range of
  Range24h -> (addUTCTime (-(24 * 3_600)) now, ListenerSnapshots.FiveMinute)
  Range7d -> (addUTCTime (-(7 * 86_400)) now, ListenerSnapshots.Hourly)
  Range30d -> (addUTCTime (-(30 * 86_400)) now, ListenerSnapshots.Hourly)
  Range90d -> (addUTCTime (-(90 * 86_400)) now, ListenerSnapshots.Daily)

-- | Format a UTCTime as ISO 8601 for chart labels.
formatUTC :: UTCTime -> Text
formatUTC = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

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
