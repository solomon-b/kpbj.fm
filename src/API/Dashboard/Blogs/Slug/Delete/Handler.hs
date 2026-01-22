{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler _tracer showSlug postId cookie =
  handleBannerErrors "Show blog post delete" $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie

    -- 2. Fetch show and blog post
    showModel <- fetchShow showSlug
    blogPost <- fetchBlogPost postId

    -- 3. Verify blog post belongs to show
    unless (blogPost.showId == showModel.id) $
      throwNotFound "Blog post"

    -- 4. Check authorization
    isHost <- checkIsHost user userMetadata showModel.id
    let isAuthorized = isHost && not (UserMetadata.isSuspended userMetadata)
    unless isAuthorized $
      throwNotAuthorized "You don't have permission to delete this blog post." (Just userMetadata.mUserRole)

    -- 5. Delete the blog post
    deleteBlogPost blogPost

--------------------------------------------------------------------------------
-- Helpers

fetchShow ::
  Slug ->
  AppM Shows.Model
fetchShow showSlug =
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

fetchBlogPost ::
  ShowBlogPosts.Id ->
  AppM ShowBlogPosts.Model
fetchBlogPost postId =
  execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just post) -> pure post

checkIsHost ::
  User.Model ->
  UserMetadata.Model ->
  Shows.Id ->
  AppM Bool
checkIsHost user userMetadata showId
  | UserMetadata.isStaffOrHigher userMetadata.mUserRole = pure True
  | otherwise = execQuerySpan (ShowHost.isUserHostOfShow user.mId showId) >>= either (const $ pure False) pure

deleteBlogPost ::
  ShowBlogPosts.Model ->
  AppM (Lucid.Html ())
deleteBlogPost blogPost =
  execQuerySpan (ShowBlogPosts.deleteShowBlogPost blogPost.id) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just _) -> do
      Log.logInfo "Blog post deleted successfully" blogPost.id
      pure $ do
        Lucid.toHtmlRaw ("" :: Text)
        renderBanner Success "Blog Post Deleted" "The blog post has been deleted."
