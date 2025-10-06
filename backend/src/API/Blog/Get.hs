module API.Blog.Get where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.Error (errorTemplate)
import API.Blog.Get.Templates.Page (template)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as Blog
import Effects.Database.Tables.BlogTags qualified as BlogTag
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
    "GET /blog"
    ( "blog"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "category" Text
        :> Servant.QueryParam "tag" Text
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
  Maybe Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer maybePage maybeCategory maybeTag cookie hxRequest = do
  let page = fromMaybe 1 maybePage
      limit = 10
      offset = (page - 1) * limit
      isHtmxRequest = checkHtmxRequest hxRequest

  -- Get user info once upfront
  loginState <- Auth.userLoginState cookie
  mUserInfo <- case loginState of
    Auth.IsNotLoggedIn -> pure Nothing
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right userMetadata -> pure userMetadata
        _ -> pure Nothing

  -- Get sidebar data
  tagsResult <- execQuerySpan BlogTag.getTagsWithCounts
  categoriesResult <- execQuerySpan BlogTag.getCategoriesWithCounts

  let tagsWithCounts = fromRight [] tagsResult
      categoriesWithCounts = fromRight [] categoriesResult

  -- Get blog posts based on filters (category or tag)
  blogPostsResult <- case (maybeCategory, maybeTag) of
    (Just category, _) ->
      execQuerySpan (Blog.getBlogPostsByCategory category limit offset)
    (_, Just tagName) ->
      execQuerySpan (BlogTag.getTagByName tagName) >>= \case
        Left err ->
          pure (Left err)
        Right Nothing ->
          pure (Right [])
        Right (Just tag) ->
          execQuerySpan (Blog.getPostsByTag (BlogTag.btmId tag) limit offset)
    (Nothing, Nothing) ->
      execQuerySpan (Blog.getPublishedBlogPosts limit offset)
  case blogPostsResult of
    Left _err -> do
      Log.logInfo "Failed to fetch blog posts from database" ()
      renderTemplate isHtmxRequest mUserInfo (errorTemplate "Failed to load blog posts. Please try again.")
    Right allPosts -> do
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit
          blogTemplate = template posts page hasMore tagsWithCounts categoriesWithCounts
      renderTemplate isHtmxRequest mUserInfo blogTemplate
