{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Episodes.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (..), hoursToTimeZone, utcToLocalTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the episode list page.
data EpisodeListViewData = EpisodeListViewData
  { elvUserMetadata :: UserMetadata.Model,
    elvAllShows :: [Shows.Model],
    elvSelectedShow :: Maybe Shows.Model,
    elvEpisodes :: [Episodes.Model],
    elvSchedules :: [ShowSchedule.ScheduleTemplate Result],
    elvNextShow :: Maybe ShowSchedule.UpcomingShowDate,
    elvPage :: Int64,
    elvHasMore :: Bool
  }

-- | Business logic: pagination setup, show/episode fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  Maybe Int64 ->
  ExceptT HandlerError AppM EpisodeListViewData
action user userMetadata showSlug maybePage = do
  -- 1. Set up pagination (use Pacific time for "today")
  nowUtc <- liftIO getCurrentTime
  let pacificTz = hoursToTimeZone (-8) -- PST is UTC-8
      nowPacific = utcToLocalTime pacificTz nowUtc
      today = localDay nowPacific
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  -- 2. Fetch shows for sidebar (admins see all, hosts see their own)
  allShows <- lift $ fetchShowsForUser user userMetadata

  -- 3. Determine which show to display
  let selectedShow = find (\s -> s.slug == showSlug) allShows
  case selectedShow of
    Nothing ->
      pure
        EpisodeListViewData
          { elvUserMetadata = userMetadata,
            elvAllShows = allShows,
            elvSelectedShow = Nothing,
            elvEpisodes = [],
            elvSchedules = [],
            elvNextShow = Nothing,
            elvPage = page,
            elvHasMore = False
          }
    Just showToFetch -> do
      -- 4. Fetch episodes for the show
      (allEpisodes, schedules, nextShow) <- fetchEpisodesData today showToFetch limit offset
      let episodes = take (fromIntegral limit) allEpisodes
          hasMore = length allEpisodes > fromIntegral limit
      pure
        EpisodeListViewData
          { elvUserMetadata = userMetadata,
            elvAllShows = allShows,
            elvSelectedShow = Just showToFetch,
            elvEpisodes = episodes,
            elvSchedules = schedules,
            elvNextShow = nextShow,
            elvPage = page,
            elvHasMore = hasMore
          }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata showSlug maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.elvPage > 1
    case (isAppendRequest, vd.elvSelectedShow) of
      (True, Just showToFetch) ->
        -- Infinite scroll: return only new rows + sentinel
        pure $ renderItemsFragment vd.elvUserMetadata showToFetch vd.elvEpisodes vd.elvPage vd.elvHasMore
      _ -> do
        -- Full page: render with table, sentinel, and noscript pagination
        let content = template vd.elvUserMetadata vd.elvSelectedShow vd.elvEpisodes vd.elvPage vd.elvHasMore
        lift $
          renderDashboardTemplate
            hxRequest
            vd.elvUserMetadata
            vd.elvAllShows
            vd.elvSelectedShow
            NavEpisodes
            (statsContent vd.elvUserMetadata vd.elvEpisodes vd.elvSchedules vd.elvNextShow)
            (fmap actionButton vd.elvSelectedShow)
            content
  where
    actionButton :: Shows.Model -> Lucid.Html ()
    actionButton showModel =
      let uploadUrl = Links.linkURI $ dashboardShowsLinks.episodeNewGet showModel.slug
       in Lucid.a_
            [ Lucid.href_ [i|/#{uploadUrl}|],
              hxGet_ [i|/#{uploadUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
            ]
            "New Episode"

    statsContent ::
      UserMetadata.Model ->
      [Episodes.Model] ->
      [ShowSchedule.ScheduleTemplate Result] ->
      Maybe ShowSchedule.UpcomingShowDate ->
      Maybe (Lucid.Html ())
    statsContent userMeta episodes schedules nextShow =
      Just $ do
        -- Schedule info (only for hosts, not admins)
        let isAdmin = UserMetadata.isAdmin userMeta.mUserRole
        if not isAdmin
          then do
            renderScheduleInfo schedules
            case nextShow of
              Just upcoming -> do
                Lucid.span_ [Lucid.class_ "mx-2"] "•"
                Lucid.span_ [] $ do
                  "Next: "
                  Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d" (ShowSchedule.usdShowDate upcoming)
              Nothing -> mempty
            Lucid.span_ [Lucid.class_ "mx-2"] "•"
          else mempty
        Lucid.span_ [] $ Lucid.toHtml $ show (length episodes) <> " episodes"

    renderScheduleInfo :: [ShowSchedule.ScheduleTemplate Result] -> Lucid.Html ()
    renderScheduleInfo [] = Lucid.span_ [] "Not scheduled"
    renderScheduleInfo (firstSchedule : rest) =
      let allSchedules = firstSchedule : rest
          dayNames = mapMaybe (fmap display . ShowSchedule.stDayOfWeek) allSchedules
          dayText = if null dayNames then "One-time" else Text.intercalate ", " dayNames
          timeRange = formatTimeOfDay firstSchedule.stStartTime <> "-" <> formatTimeOfDay firstSchedule.stEndTime
       in Lucid.span_ [] $ Lucid.toHtml $ dayText <> " " <> timeRange

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

-- | Fetch episodes data for a show with pagination
fetchEpisodesData ::
  Day ->
  Shows.Model ->
  Limit ->
  Offset ->
  ExceptT HandlerError AppM ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesData today showModel limit offset =
  fromRightM throwDatabaseError $
    execTransaction (fetchEpisodesDataPaginated today showModel limit offset)

-- | Fetch episodes data for dashboard with pagination
fetchEpisodesDataPaginated :: Day -> Shows.Model -> Limit -> Offset -> Txn.Transaction ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesDataPaginated date showModel lim off = do
  -- Fetch limit + 1 to check if there are more results
  episodes <- Txn.statement () (Episodes.getEpisodesForShow showModel.id (lim + 1) off)
  schedules <- Txn.statement () (ShowSchedule.getScheduleTemplatesForShow showModel.id)
  upcomingShows <- Txn.statement () (ShowSchedule.getUpcomingShowDates showModel.id date 1)
  let nextShow = listToMaybe upcomingShows
  pure (episodes, schedules, nextShow)
