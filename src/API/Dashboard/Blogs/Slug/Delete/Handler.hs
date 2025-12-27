{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.Handler where

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
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

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
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer showSlug postId cookie = do
  -- Fetch the show by slug
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      pure $ renderBanner Error "Delete Failed" "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Delete failed: Show not found" (Aeson.object ["showSlug" .= showSlug])
      pure $ renderBanner Error "Delete Failed" "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          pure $ renderBanner Error "Delete Failed" "You must be logged in to delete blog posts."
        Just (user, userMetadata) -> do
          execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
            Left err -> do
              Log.logInfo "Delete failed: Failed to fetch blog post" (Aeson.object ["error" .= show err])
              pure $ renderBanner Error "Delete Failed" "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Delete failed: Blog post not found" (Aeson.object ["postId" .= postId])
              pure $ renderBanner Error "Delete Failed" "Blog post not found."
            Right (Just blogPost) -> do
              -- Verify the blog post belongs to the show
              if blogPost.showId /= showModel.id
                then do
                  Log.logInfo "Delete failed: Blog post does not belong to show" (Aeson.object ["postId" .= postId, "showSlug" .= showSlug])
                  pure $ renderBanner Error "Delete Failed" "Blog post not found."
                else do
                  isHostOrStaff <- checkIfHost userMetadata user showModel.id

                  if isHostOrStaff && not (UserMetadata.isSuspended userMetadata)
                    then deleteBlogPost blogPost
                    else do
                      Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "postId" .= blogPost.id])
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
  ShowBlogPosts.Model ->
  m (Lucid.Html ())
deleteBlogPost blogPost = do
  execQuerySpan (ShowBlogPosts.deleteShowBlogPost blogPost.id) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "postId" .= blogPost.id])
      pure $ renderBanner Error "Delete Failed" "Failed to delete blog post due to a database error."
    Right Nothing -> do
      Log.logInfo "Delete failed: Blog post not found during delete" (Aeson.object ["postId" .= blogPost.id])
      pure $ renderBanner Error "Delete Failed" "Blog post not found during delete operation."
    Right (Just _) -> do
      Log.logInfo "Blog post deleted successfully" (Aeson.object ["postId" .= blogPost.id])
      -- Return empty response so the post card gets removed
      pure $ Lucid.toHtmlRaw ("" :: Text)

checkIfHost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  UserMetadata.Model ->
  User.Model ->
  Shows.Id ->
  m Bool
checkIfHost userMetadata user showId
  | UserMetadata.isStaffOrHigher userMetadata.mUserRole = pure True
  | UserMetadata.isSuspended userMetadata = pure False
  | otherwise = execQuerySpan (ShowHost.isUserHostOfShow user.mId showId) >>= either (const $ pure False) pure
