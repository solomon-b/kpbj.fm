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
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (Day, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.DayOfWeek (dayOfWeekToText)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Slug ->
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer showSlug maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes list" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination
    today <- liftIO $ utctDay <$> getCurrentTime
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar (admins see all, hosts see their own)
    allShows <- fetchShowsForUser user userMetadata

    -- 4. Determine which show to display
    let selectedShow = find (\s -> s.slug == showSlug) allShows
    case selectedShow of
      Nothing -> do
        -- No matching show found - render empty state
        let content = template userMetadata Nothing [] page False
        renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes (statsContent userMetadata [] [] Nothing) Nothing content
      Just showToFetch -> do
        -- 5. Fetch episodes for the show
        (allEpisodes, schedules, nextShow) <- fetchEpisodesData today showToFetch limit offset

        let episodes = take (fromIntegral limit) allEpisodes
            hasMore = length allEpisodes > fromIntegral limit

        if isAppendRequest
          then
            -- Infinite scroll: return only new rows + sentinel
            pure $ renderItemsFragment userMetadata showToFetch episodes page hasMore
          else do
            -- Full page: render with table, sentinel, and noscript pagination
            let content = template userMetadata (Just showToFetch) episodes page hasMore
            renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes (statsContent userMetadata episodes schedules nextShow) (actionButton showToFetch) content
  where
    actionButton :: Shows.Model -> Maybe (Lucid.Html ())
    actionButton showModel =
      let uploadUrl = Links.linkURI $ dashboardShowsLinks.episodeNewGet showModel.slug
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{uploadUrl}|],
                hxGet_ [i|/#{uploadUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
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
          dayNames = mapMaybe (fmap dayOfWeekToText . (.stDayOfWeek)) allSchedules
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
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

-- | Fetch episodes data for a show with pagination
fetchEpisodesData ::
  Day ->
  Shows.Model ->
  Limit ->
  Offset ->
  AppM ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesData today showModel limit offset =
  execTransactionSpan (fetchEpisodesDataPaginated today showModel limit offset) >>= \case
    Left err -> throwDatabaseError err
    Right result -> pure result

-- | Fetch episodes data for dashboard with pagination
fetchEpisodesDataPaginated :: Day -> Shows.Model -> Limit -> Offset -> Txn.Transaction ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesDataPaginated date showModel lim off = do
  -- Fetch limit + 1 to check if there are more results
  episodes <- Txn.statement () (Episodes.getEpisodesForShowIncludingDrafts showModel.id (lim + 1) off)
  schedules <- Txn.statement () (ShowSchedule.getScheduleTemplatesForShow showModel.id)
  upcomingShows <- Txn.statement () (ShowSchedule.getUpcomingShowDates showModel.id date 1)
  let nextShow = listToMaybe upcomingShows
  pure (episodes, schedules, nextShow)
