module API.Blog.Post.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink)
import API.Blog.Post.Get.Templates.Page (notFoundTemplate, template)
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
import Effects.Database.Tables.Blog qualified as Blog
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing Nothing (Just tag)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /blog/:slug"
    ( "blog"
        :> Servant.Capture "slug" Text
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

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

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
  Text ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer slug cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  -- Get user info once upfront
  loginState <- Auth.userLoginState cookie
  mUserInfo <- case loginState of
    Auth.IsNotLoggedIn -> pure Nothing
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right userMetadata ->
          pure userMetadata
        _ -> pure Nothing

  execQuerySpan (Blog.getBlogPostBySlug slug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog post from database" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Blog post not found" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just blogPost) -> do
      execQuerySpan (UserMetadata.getUserMetadata (Blog.bpmAuthorId blogPost)) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch blog post author" (Blog.bpmAuthorId blogPost)
          renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
        Right Nothing -> do
          Log.logInfo "Blog post author not found" (Blog.bpmAuthorId blogPost)
          renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
        Right (Just author) -> do
          tagsResult <- execQuerySpan (Blog.getTagsForPost (Blog.bpmId blogPost))
          let tags = case tagsResult of
                Left _err -> []
                Right tagModels -> tagModels
              postTemplate = template blogPost author tags
          renderTemplate isHtmxRequest mUserInfo postTemplate
