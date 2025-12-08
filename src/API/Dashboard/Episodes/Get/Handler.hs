{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.Page (template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Links (showEpisodesLinks)
import API.Types
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (find, uncons)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (Day, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.DayOfWeek (dayOfWeekToText)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug cookie (foldHxReq -> hxRequest) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard episodes" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard episodes" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (_, userMetadata)
      -- Admins see all shows, hosts see their assigned shows
      | UserMetadata.isAdmin userMetadata.mUserRole -> do
          Log.logInfo "Admin accessing dashboard episodes" userMetadata.mDisplayName
          execQuerySpan Shows.getAllActiveShows >>= \case
            Left _err -> do
              let content = template userMetadata Nothing []
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes (statsContent userMetadata [] [] Nothing) Nothing content
            Right allShows -> do
              let selectedShow = find (\s -> s.slug == showSlug) allShows
              case selectedShow of
                Nothing -> do
                  case uncons allShows of
                    Nothing ->
                      execQuerySpan Episodes.getAllEpisodes >>= \case
                        Left _err -> do
                          let content = template userMetadata Nothing []
                          renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes (statsContent userMetadata [] [] Nothing) Nothing content
                        Right allEpisodes -> do
                          let content = template userMetadata Nothing allEpisodes
                          renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes (statsContent userMetadata allEpisodes [] Nothing) Nothing content
                    Just (showToFetch, _) -> do
                      execTransactionSpan (fetchEpisodesData today showToFetch) >>= \case
                        Left _err -> do
                          let content = template userMetadata (Just showToFetch) []
                          renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes (statsContent userMetadata [] [] Nothing) (actionButton showToFetch) content
                        Right (episodes, schedules, nextShow) -> do
                          let content = template userMetadata (Just showToFetch) episodes
                          renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes (statsContent userMetadata episodes schedules nextShow) (actionButton showToFetch) content
                Just showToFetch -> do
                  execTransactionSpan (fetchEpisodesData today showToFetch) >>= \case
                    Left _err -> do
                      let content = template userMetadata (Just showToFetch) []
                      renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes (statsContent userMetadata [] [] Nothing) (actionButton showToFetch) content
                    Right (episodes, schedules, nextShow) -> do
                      let content = template userMetadata (Just showToFetch) episodes
                      renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes (statsContent userMetadata episodes schedules nextShow) (actionButton showToFetch) content
    Just (user, userMetadata) -> do
      Log.logInfo "Host accessing dashboard episodes" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          let content = template userMetadata Nothing []
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes (statsContent userMetadata [] [] Nothing) Nothing content
        Right userShows -> do
          let selectedShow = find (\s -> s.slug == showSlug) userShows
          case selectedShow of
            Nothing -> do
              let content = template userMetadata Nothing []
              renderDashboardTemplate hxRequest userMetadata userShows Nothing NavEpisodes (statsContent userMetadata [] [] Nothing) Nothing content
            Just showToFetch -> do
              execTransactionSpan (fetchEpisodesData today showToFetch) >>= \case
                Left _err -> do
                  let content = template userMetadata (Just showToFetch) []
                  renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavEpisodes (statsContent userMetadata [] [] Nothing) (actionButton showToFetch) content
                Right (episodes, schedules, nextShow) -> do
                  let content = template userMetadata (Just showToFetch) episodes
                  renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavEpisodes (statsContent userMetadata episodes schedules nextShow) (actionButton showToFetch) content
  where
    actionButton :: Shows.Model -> Maybe (Lucid.Html ())
    actionButton showModel =
      let uploadUrl = Links.linkURI $ showEpisodesLinks.newGet showModel.slug
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{uploadUrl}|],
                hxGet_ [i|/#{uploadUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
              ]
              "New Episode"

    -- \| Build stats content for top bar
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

-- | Fetch episodes data for dashboard
fetchEpisodesData :: Day -> Shows.Model -> Txn.Transaction ([Episodes.Model], [ShowSchedule.ScheduleTemplate Result], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesData date showModel = do
  episodes <- Txn.statement () (Episodes.getEpisodesById showModel.id)
  schedules <- Txn.statement () (ShowSchedule.getScheduleTemplatesForShow showModel.id)
  upcomingShows <- Txn.statement () (ShowSchedule.getUpcomingShowDates showModel.id date 1)
  let nextShow = listToMaybe upcomingShows
  pure (episodes, schedules, nextShow)
