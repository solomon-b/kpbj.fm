{-# LANGUAGE ViewPatterns #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.Error (errorTemplate)
import API.Blog.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
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
  Maybe Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage maybeCategory maybeTag cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      limit = 10
      offset = (page - 1) * limit

  -- Get user info once upfront
  mUserInfo <- getUserInfo cookie <&> fmap snd

  -- Get sidebar data
  tagsResult <- execQuerySpan BlogTags.getTagsWithCounts
  categoriesResult <- execQuerySpan BlogTags.getCategoriesWithCounts

  let tagsWithCounts = fromRight [] tagsResult
      categoriesWithCounts = fromRight [] categoriesResult

  -- Get blog posts based on filters (category or tag)
  blogPostsResult <- getBlogPostResults limit offset maybeCategory maybeTag

  case blogPostsResult of
    Left _err -> do
      Log.logInfo "Failed to fetch blog posts from database" ()
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load blog posts. Please try again.")
    Right allPosts -> do
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit
          blogTemplate = template posts page hasMore tagsWithCounts categoriesWithCounts
      renderTemplate hxRequest mUserInfo blogTemplate

getBlogPostResults ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Int64 ->
  Int64 ->
  Maybe Text ->
  Maybe Text ->
  m (Either HSQL.Pool.UsageError [BlogPosts.Model])
getBlogPostResults limit offset maybeCategory maybeTag = do
  case (maybeCategory, maybeTag) of
    (Just category, _) ->
      execQuerySpan (BlogPosts.getBlogPostsByCategory category limit offset)
    (_, Just tagName) ->
      execQuerySpan (BlogTags.getTagByName tagName) >>= \case
        Left err ->
          pure (Left err)
        Right Nothing ->
          pure (Right [])
        Right (Just tag) ->
          execQuerySpan (BlogPosts.getPostsByTag (BlogTags.btmId tag) limit offset)
    (Nothing, Nothing) ->
      execQuerySpan (BlogPosts.getPublishedBlogPosts limit offset)
