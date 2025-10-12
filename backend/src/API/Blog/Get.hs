{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.Error (errorTemplate)
import API.Blog.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (forM)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage maybeTag cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      limit = 10
      offset = (page - 1) * limit

  -- Get user info once upfront
  mUserInfo <- getUserInfo cookie <&> fmap snd

  -- Get blog posts based on filters (tag only)
  blogPostsResult <- getBlogPostResults limit offset maybeTag

  case blogPostsResult of
    Left _err -> do
      Log.logInfo "Failed to fetch blog posts from database" ()
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load blog posts. Please try again.")
    Right allPosts -> do
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit
          blogTemplate = template posts page hasMore
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
  m (Either HSQL.Pool.UsageError [(BlogPosts.Model, [BlogTags.Model])])
getBlogPostResults limit offset maybeTag = do
  case maybeTag of
    Just tagName ->
      execQuerySpan (BlogTags.getTagByName tagName) >>= \case
        Left err ->
          pure (Left err)
        Right Nothing ->
          pure (Right [])
        Right (Just tag) ->
          getPostsWithTagsFiltered tag limit offset
    Nothing ->
      getPostsWithTags limit offset

getPostsWithTags ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Int64 ->
  Int64 ->
  m (Either HSQL.Pool.UsageError [(BlogPosts.Model, [BlogTags.Model])])
getPostsWithTags limit offset =
  execTransactionSpan $ do
    posts <- HT.statement () $ BlogPosts.getPublishedBlogPosts limit offset
    forM posts $ \post -> do
      tags <- HT.statement () $ BlogPosts.getTagsForPost post.bpmId
      pure (post, tags)

getPostsWithTagsFiltered ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  BlogTags.Model ->
  Int64 ->
  Int64 ->
  m (Either HSQL.Pool.UsageError [(BlogPosts.Model, [BlogTags.Model])])
getPostsWithTagsFiltered tag limit offset =
  execTransactionSpan $ do
    posts <- HT.statement () $ BlogPosts.getPostsByTag tag.btmId limit offset
    forM posts $ \post -> do
      tags <- HT.statement () $ BlogPosts.getTagsForPost post.bpmId
      pure (post, tags)
