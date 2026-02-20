{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Get.Templates.Page (editBlogPostForm)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug postId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Blog post edit form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to edit blog posts." userMetadata
    vd <- action user userMetadata showSlug postId
    lift $ renderDashboardTemplate hxRequest vd.bevUserMetadata vd.bevAllShows (Just vd.bevShowModel) NavBlog Nothing Nothing $ editBlogPostForm vd.bevShowModel vd.bevBlogPost vd.bevTags

--------------------------------------------------------------------------------

-- | All data needed to render the blog post edit form.
data BlogEditViewData = BlogEditViewData
  { bevUserMetadata :: UserMetadata.Model,
    bevAllShows :: [Shows.Model],
    bevShowModel :: Shows.Model,
    bevBlogPost :: ShowBlogPosts.Model,
    bevTags :: [ShowBlogTags.Model]
  }

-- | Business logic: post context fetch, authorization check, sidebar shows.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM BlogEditViewData
action user userMetadata showSlug postId = do
  -- 1. Fetch blog post, show, tags, and verify host permissions in a transaction
  (post, showModel, tags, isHost) <- fetchBlogPostWithContext user userMetadata showSlug postId

  -- 2. Verify user is authorized to edit this post
  if not (isHost || User.mId user == post.authorId || UserMetadata.isStaffOrHigher userMetadata.mUserRole)
    then throwNotAuthorized "You are not authorized to edit this blog post." (Just userMetadata.mUserRole)
    else do
      Log.logInfo "Authorized user accessing blog post edit form" post.id

      -- 3. Fetch shows for dashboard sidebar
      allShows <-
        lift $
          if UserMetadata.isAdmin userMetadata.mUserRole
            then fromRight [] <$> execQuery Shows.getAllActiveShows
            else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

      pure
        BlogEditViewData
          { bevUserMetadata = userMetadata,
            bevAllShows = allShows,
            bevShowModel = showModel,
            bevBlogPost = post,
            bevTags = tags
          }

fetchBlogPostWithContext ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM (ShowBlogPosts.Model, Shows.Model, [ShowBlogTags.Model], Bool)
fetchBlogPostWithContext user userMetadata showSlug postId = do
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          post <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostById postId)
          showModel <- MaybeT $ HT.statement () (Shows.getShowById post.showId)
          -- Verify the show slug matches
          MaybeT $ pure $ if showModel.slug == showSlug then Just () else Nothing
          tags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost post.id)
          -- Staff and admins don't need explicit host check since they have access to all shows
          isHost <-
            if UserMetadata.isStaffOrHigher userMetadata.mUserRole
              then pure True
              else lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) post.showId)
          pure (post, showModel, tags, isHost)

  case mResult of
    Nothing -> throwNotFound "Blog post"
    Just result -> pure result
