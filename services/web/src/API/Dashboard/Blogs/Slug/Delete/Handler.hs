{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + banner response.
handler ::
  Slug ->
  ShowBlogPosts.Id ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  AppM (Lucid.Html ())
handler showSlug postId host =
  handleBannerErrors "Show blog post delete" $ do
    showModel <- authorizeShow host showSlug
    action showModel postId
    pure $ do
      Lucid.toHtmlRaw ("" :: Text)
      renderBanner Success "Blog Post Deleted" "The blog post has been deleted."

--------------------------------------------------------------------------------

-- | Business logic: post fetch, ownership check, delete, for a show that
-- 'authorizeShow' checked.
action ::
  Shows.Model ->
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM ()
action showModel postId = do
  -- 1. Fetch the blog post
  blogPost <- fetchBlogPost postId

  -- 2. Verify blog post belongs to show
  unless (blogPost.showId == showModel.id) $
    throwNotFound "Blog post"

  -- 3. Delete the blog post
  _ <-
    fromMaybeM (throwNotFound "Blog post") $
      fromRightM throwDatabaseError $
        execQuery (ShowBlogPosts.deleteShowBlogPost blogPost.id)
  Log.logInfo "Blog post deleted successfully" blogPost.id

--------------------------------------------------------------------------------
-- Helpers

fetchBlogPost ::
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM ShowBlogPosts.Model
fetchBlogPost postId =
  fromMaybeM (throwNotFound "Blog post") $
    fromRightM throwDatabaseError $
      execQuery (ShowBlogPosts.getShowBlogPostById postId)
