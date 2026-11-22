{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Episodes.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.CurrentShow (fetchDashboardShows)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the episode list page.
data EpisodeListViewData = EpisodeListViewData
  { elvUserMetadata :: UserMetadata.Model,
    elvSelectedShow :: Maybe Shows.Model,
    elvEpisodes :: [Episodes.Model],
    elvSchedules :: [ShowSchedule.ScheduleTemplate Result],
    elvNextShow :: Maybe ShowSchedule.UpcomingShowDate,
    elvPage :: Int64,
    elvHasMore :: Bool
  }

-- | Business logic: pagination setup, show/episode fetching.
action ::
  UserMetadata.Model ->
  Slug ->
  Maybe Int64 ->
  ExceptT HandlerError AppM EpisodeListViewData
action userMetadata showSlug maybePage = do
  -- 1. Set up pagination
  today <- pacificDay <$> liftIO getCurrentTime
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  -- 2. Fetch the shows this user may open. This list is the access check: a
  -- slug outside it renders the page with no show.
  accessibleShows <- lift $ fetchDashboardShows userMetadata

  -- 3. Determine which show to display
  let selectedShow = find (\s -> s.slug == showSlug) accessibleShows
  case selectedShow of
    Nothing ->
      pure
        EpisodeListViewData
          { elvUserMetadata = userMetadata,
            elvSelectedShow = Nothing,
            elvEpisodes = [],
            elvSchedules = [],
            elvNextShow = Nothing,
            elvPage = page,
            elvHasMore = False
          }
    Just showToFetch -> do
      -- 4. Fetch episodes for the show. Staff and admins moderate, so they see
      -- the archived episodes too. A host sees the live ones only.
      (allEpisodes, schedules, nextShow) <- fetchEpisodesData today showToFetch (archivedFilterFor userMetadata) limit offset
      let episodes = take (fromIntegral limit) allEpisodes
          hasMore = length allEpisodes > fromIntegral limit
      pure
        EpisodeListViewData
          { elvUserMetadata = userMetadata,
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
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug maybePage (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata showSlug maybePage
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
            mFlash
            vd.elvUserMetadata
            (maybe NoShow ForShow vd.elvSelectedShow)
            NavEpisodes
            (statsContent vd.elvUserMetadata vd.elvEpisodes vd.elvSchedules vd.elvNextShow)
            (fmap actionButton vd.elvSelectedShow)
            content
  where
    actionButton :: Shows.Model -> Lucid.Html ()
    actionButton showModel =
      let uploadUrl = Links.linkURI $ dashboardShowsLinks.episodeNewGet showModel.slug
       in Button.render (Button.defaults & Button.primary & Button.dashboardLink [i|/#{uploadUrl}|]) "New Episode"

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
      let dayNames = map (display . ShowSchedule.stDayOfWeek) (firstSchedule : rest)
          dayText = Text.intercalate ", " dayNames
          timeRange = formatTimeOfDay firstSchedule.stStartTime <> "-" <> formatTimeOfDay firstSchedule.stEndTime
       in Lucid.span_ [] $ Lucid.toHtml $ dayText <> " " <> timeRange

-- | Which episodes a role may see in the dashboard.
--
-- Archive is the moderation tool, so staff and admins see what they archived.
-- A host sees the live episodes only.
archivedFilterFor :: UserMetadata.Model -> Episodes.ArchivedFilter
archivedFilterFor userMetadata
  | UserMetadata.isStaffOrHigher userMetadata.mUserRole = Episodes.IncludeArchived
  | otherwise = Episodes.ExcludeArchived

-- | Fetch episodes data for a show with pagination
fetchEpisodesData ::
  Day ->
  Shows.Model ->
  Episodes.ArchivedFilter ->
  Limit ->
  Offset ->
  ExceptT HandlerError AppM ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesData today showModel archived limit offset =
  fromRightM throwDatabaseError $
    execTransaction (fetchEpisodesDataPaginated today showModel archived limit offset)

-- | Fetch episodes data for dashboard with pagination
fetchEpisodesDataPaginated :: Day -> Shows.Model -> Episodes.ArchivedFilter -> Limit -> Offset -> Txn.Transaction ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesDataPaginated date showModel archived lim off = do
  -- Read the id through the field selector. hlint reads showModel.id as a
  -- composition with Data.Function.id, and reports a redundant id.
  let showId = Shows.id showModel
  -- Fetch limit + 1 to check if there are more results
  episodes <- Txn.statement () (Episodes.getEpisodesForShow showId archived (lim + 1) off)
  schedules <- Txn.statement () (ShowSchedule.getScheduleTemplatesForShow showId)
  upcomingShows <- Txn.statement () (ShowSchedule.getUpcomingShowDates showId date 1)
  let nextShow = listToMaybe upcomingShows
  pure (episodes, schedules, nextShow)
