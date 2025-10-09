module API.Host.Dashboard.Get where

--------------------------------------------------------------------------------

import API.Host.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Host.Dashboard.Get.Templates.Page (template)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
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
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

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
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  checkAuth isHtmxRequest cookie $ \user userMetadata -> do
    if UserMetadata.isHostOrHigher userMetadata.mUserRole
      then do
        Log.logInfo "Authorized user accessing host dashboard" userMetadata.mDisplayName

        -- Fetch user's shows
        userShows <- fromRightM (\_ -> pure []) $ execQuerySpan (Shows.getShowsForUser (User.mId user))

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
        renderTemplate isHtmxRequest (Just userMetadata) dashboardTemplate
      else do
        Log.logInfo "User without Host role tried to access host dashboard" userMetadata.mDisplayName
        renderTemplate isHtmxRequest (Just userMetadata) notAuthorizedTemplate

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

checkAuth ::
  ( MonadDB m,
    MonadCatch m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Bool ->
  Maybe Text ->
  (User.Model -> UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
checkAuth isHtmxRequest cookie k = do
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn -> do
      Log.logInfo "Unauthorized access to host dashboard" ()
      renderTemplate isHtmxRequest Nothing notLoggedInTemplate
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right (Just userMetadata) -> k user userMetadata
        _ -> do
          Log.logInfo "Failed to fetch user metadata for host dashboard" user.mId
          renderTemplate isHtmxRequest Nothing notLoggedInTemplate
