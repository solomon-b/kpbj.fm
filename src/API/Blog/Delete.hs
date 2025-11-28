{-# LANGUAGE OverloadedRecordDot #-}

module API.Blog.Delete where

--------------------------------------------------------------------------------

import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
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
    "DELETE /blog/:post_id/:post_slug"
    ( "blog"
        :> Servant.Capture "post_id" BlogPosts.Id
        :> Servant.Capture "post_slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
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
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer postId _postSlug cookie = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo_ "No user session"
      pure $ renderBanner Error "Delete Failed" "You must be logged in to delete blog posts."
    Just (user, userMetadata) -> do
      execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
        Left err -> do
          Log.logInfo "Delete failed: Failed to fetch blog post" (Aeson.object ["error" .= show err])
          pure $ renderBanner Error "Delete Failed" "Database error. Please try again or contact support."
        Right Nothing -> do
          Log.logInfo "Delete failed: Blog post not found" (Aeson.object ["postId" .= postId])
          pure $ renderBanner Error "Delete Failed" "Blog post not found."
        Right (Just blogPost) -> do
          -- Check authorization: must be staff or admin, or the author
          let isAuthor = blogPost.bpmAuthorId == user.mId
              isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole

          if isAuthor || isStaffOrAdmin
            then deleteBlogPost blogPost
            else do
              Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "postId" .= blogPost.bpmId])
              pure $ renderBanner Error "Delete Failed" "You don't have permission to delete this blog post."

deleteBlogPost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  BlogPosts.Model ->
  m (Lucid.Html ())
deleteBlogPost blogPost = do
  execQuerySpan (BlogPosts.deleteBlogPost (BlogPosts.bpmId blogPost)) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "postId" .= BlogPosts.bpmId blogPost])
      pure $ renderBanner Error "Delete Failed" "Failed to delete blog post due to a database error."
    Right Nothing -> do
      Log.logInfo "Delete failed: Blog post not found during delete" (Aeson.object ["postId" .= BlogPosts.bpmId blogPost])
      pure $ renderBanner Error "Delete Failed" "Blog post not found during delete operation."
    Right (Just _) -> do
      Log.logInfo "Blog post deleted successfully" (Aeson.object ["postId" .= BlogPosts.bpmId blogPost])
      -- Return empty response so the post card gets removed
      pure $ Lucid.toHtmlRaw ("" :: Text)
