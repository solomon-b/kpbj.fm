{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Get where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.Page (template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (Day, getCurrentTime, utctDay)
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
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/episodes"
    ( "dashboard"
        :> "episodes"
        :> Servant.QueryParam "show" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybeShowSlug cookie (foldHxReq -> hxRequest) = do
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
              let content = template userMetadata Nothing [] [] Nothing
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes content
            Right [] -> do
              let content = template userMetadata Nothing [] [] Nothing
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes content
            Right allShows@(firstShow : _) -> do
              let showToFetch = findShow firstShow allShows maybeShowSlug
              execTransactionSpan (fetchEpisodesData today showToFetch) >>= \case
                Left _err -> do
                  let content = template userMetadata (Just showToFetch) [] [] Nothing
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes content
                Right (episodes, schedules, nextShow) -> do
                  let content = template userMetadata (Just showToFetch) episodes schedules nextShow
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavEpisodes content
    Just (user, userMetadata) -> do
      Log.logInfo "Host accessing dashboard episodes" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          let content = template userMetadata Nothing [] [] Nothing
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes content
        Right [] -> do
          let content = template userMetadata Nothing [] [] Nothing
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes content
        Right userShows@(firstShow : _) -> do
          let showToFetch = findShow firstShow userShows maybeShowSlug
          execTransactionSpan (fetchEpisodesData today showToFetch) >>= \case
            Left _err -> do
              let content = template userMetadata (Just showToFetch) [] [] Nothing
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavEpisodes content
            Right (episodes, schedules, nextShow) -> do
              let content = template userMetadata (Just showToFetch) episodes schedules nextShow
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavEpisodes content

-- | Fetch episodes data for dashboard
fetchEpisodesData :: Day -> Shows.Model -> Txn.Transaction ([Episodes.Model], [ShowSchedule.ScheduleTemplate], Maybe ShowSchedule.UpcomingShowDate)
fetchEpisodesData date showModel = do
  episodes <- Txn.statement () (Episodes.getEpisodesById showModel.id)
  schedules <- Txn.statement () (ShowSchedule.getScheduleTemplatesForShow showModel.id)
  upcomingShows <- Txn.statement () (ShowSchedule.getUpcomingShowDates showModel.id date 1)
  let nextShow = listToMaybe upcomingShows
  pure (episodes, schedules, nextShow)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
