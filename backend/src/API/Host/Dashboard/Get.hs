{-# LANGUAGE ViewPatterns #-}

module API.Host.Dashboard.Get where

--------------------------------------------------------------------------------

import API.Host.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Host.Dashboard.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
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
    "GET /host/dashboard"
    ( "host"
        :> "dashboard"
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
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to host dashboard" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      if UserMetadata.isHostOrHigher userMetadata.mUserRole
        then do
          Log.logInfo "Authorized user accessing host dashboard" userMetadata.mDisplayName

          -- Fetch all user's shows
          execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
            Left _err -> do
              let dashboardTemplate = template userMetadata [] Nothing [] []
              renderTemplate hxRequest (Just userMetadata) dashboardTemplate
            Right [] -> do
              -- No shows assigned
              let dashboardTemplate = template userMetadata [] Nothing [] []
              renderTemplate hxRequest (Just userMetadata) dashboardTemplate
            Right userShows@(firstShow : _) -> do
              let showToFetch = findShow firstShow userShows maybeShowSlug

              execTransactionSpan (fetchDashboardData showToFetch) >>= \case
                Left _err -> do
                  -- Failed to fetch data, render empty dashboard
                  let dashboardTemplate = template userMetadata userShows (Just showToFetch) [] []
                  renderTemplate hxRequest (Just userMetadata) dashboardTemplate
                Right (episodes, blogPosts) -> do
                  let dashboardTemplate = template userMetadata userShows (Just showToFetch) episodes blogPosts
                  renderTemplate hxRequest (Just userMetadata) dashboardTemplate
        else do
          Log.logInfo "User without Host role tried to access host dashboard" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

-- | Fetch all dashboard data in a single read-only transaction
fetchDashboardData :: Shows.Model -> Txn.Transaction ([Episodes.Model], [ShowBlogPosts.Model])
fetchDashboardData showModel = do
  episodes <- Txn.statement () (Episodes.getEpisodesById showModel.id)
  blogPosts <- Txn.statement () (ShowBlogPosts.getPublishedShowBlogPosts showModel.id 10 0)
  pure (episodes, blogPosts)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
