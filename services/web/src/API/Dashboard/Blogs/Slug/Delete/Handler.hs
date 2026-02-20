{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + banner response.
handler ::
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler showSlug postId cookie =
  handleBannerErrors "Show blog post delete" $ do
    (user, userMetadata) <- requireAuth cookie
    action user userMetadata showSlug postId
    pure $ do
      Lucid.toHtmlRaw ("" :: Text)
      renderBanner Success "Blog Post Deleted" "The blog post has been deleted."

--------------------------------------------------------------------------------

-- | Business logic: show fetch, post fetch, ownership check, delete.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM ()
action user userMetadata showSlug postId = do
  -- 1. Fetch show and blog post
  showModel <- fetchShow showSlug
  blogPost <- fetchBlogPost postId

  -- 2. Verify blog post belongs to show
  unless (blogPost.showId == showModel.id) $
    throwNotFound "Blog post"

  -- 3. Check authorization (host of show or staff+, not suspended)
  requireShowHostOrStaff user.mId showSlug userMetadata

  -- 4. Delete the blog post
  _ <-
    fromMaybeM (throwNotFound "Blog post") $
      fromRightM throwDatabaseError $
        execQuery (ShowBlogPosts.deleteShowBlogPost blogPost.id)
  Log.logInfo "Blog post deleted successfully" blogPost.id

--------------------------------------------------------------------------------
-- Helpers

fetchShow ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShow showSlug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug showSlug)

fetchBlogPost ::
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM ShowBlogPosts.Model
fetchBlogPost postId =
  fromMaybeM (throwNotFound "Blog post") $
    fromRightM throwDatabaseError $
      execQuery (ShowBlogPosts.getShowBlogPostById postId)
