{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Get.Templates.Page (editBlogPostForm)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
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
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug postId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Blog post edit form" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to edit blog posts." userMetadata

    -- 2. Fetch blog post, show, tags, and verify host permissions in a transaction
    (post, showModel, tags, isHost) <- fetchBlogPostWithContext user userMetadata showSlug postId

    -- 3. Verify user is authorized to edit this post
    if not (isHost || User.mId user == post.authorId)
      then throwNotAuthorized "You are not authorized to edit this blog post."
      else do
        Log.logInfo "Authorized user accessing blog post edit form" post.id

        -- 4. Fetch shows for dashboard sidebar
        allShows <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
            else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

        -- 5. Render edit form
        renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavBlog Nothing Nothing $ editBlogPostForm showModel post tags

fetchBlogPostWithContext ::
  ( MonadUnliftIO m,
    Has Tracer env,
    MonadReader env m,
    MonadDB m,
    Log.MonadLog m,
    MonadThrow m
  ) =>
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  m (ShowBlogPosts.Model, Shows.Model, [ShowBlogTags.Model], Bool)
fetchBlogPostWithContext user userMetadata showSlug postId = do
  mResult <- execTransactionSpan $ runMaybeT $ do
    post <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostById postId)
    showModel <- MaybeT $ HT.statement () (Shows.getShowById post.showId)
    -- Verify the show slug matches
    MaybeT $ pure $ if showModel.slug == showSlug then Just () else Nothing
    tags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost post.id)
    -- Admins don't need explicit host check since they have access to all shows
    isHost <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then pure True
        else lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) post.showId)
    pure (post, showModel, tags, isHost)

  case mResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just result) -> pure result
