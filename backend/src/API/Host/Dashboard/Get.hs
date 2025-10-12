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
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to host dashboard" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      if UserMetadata.isHostOrHigher userMetadata.mUserRole
        then do
          Log.logInfo "Authorized user accessing host dashboard" userMetadata.mDisplayName

          -- Fetch user's shows
          userShows <- fromRightM (\_ -> pure []) $ execQuerySpan (Shows.getShowsForUser (User.mId user))

          -- TODO: This is wrong! We're only showing the user's first show. We shoudl support multi show users.
          -- Fetch recent episodes for user's shows
          recentEpisodes <- case userShows of
            [] -> pure []
            (primaryShow : _) -> do
              episodesResult <- execQuerySpan (Episodes.getEpisodesById primaryShow.id)
              case episodesResult of
                Left _err -> pure []
                Right episodes -> pure episodes

          -- Fetch recent blog posts by this user
          blogPostsResult <- execQuerySpan (ShowBlogPosts.getShowBlogPostsByAuthor (User.mId user) 10 0)
          blogPosts <- case blogPostsResult of
            Left _err -> pure []
            Right posts -> pure posts

          let dashboardTemplate = template userMetadata userShows recentEpisodes blogPosts
          renderTemplate hxRequest (Just userMetadata) dashboardTemplate
        else do
          Log.logInfo "User without Host role tried to access host dashboard" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m
