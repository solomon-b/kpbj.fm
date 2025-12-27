{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Get.Templates.Page (editBlogPostForm, errorTemplate, notLoggedInTemplate, permissionDeniedTemplate)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
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
handler _tracer showSlug postId cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to blog post edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- Fetch blog post, show, tags, and verify host permissions in a transaction
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
        MaybeT $ pure $ Just (post, showModel, tags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "Failed to load blog post edit form" (show err)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Failed to load blog post. Please try again."
        Right Nothing -> do
          Log.logInfo "Blog post not found" (showSlug, postId)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Blog post not found."
        Right (Just (post, showModel, tags, isHost)) -> do
          if isHost || User.mId user == post.authorId
            then do
              Log.logInfo "Authorized user accessing blog post edit form" post.id
              -- Fetch shows for dashboard sidebar
              allShows <-
                if UserMetadata.isAdmin userMetadata.mUserRole
                  then either (const []) id <$> execQuerySpan Shows.getAllActiveShows
                  else either (const []) id <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
              renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavBlog Nothing Nothing $ editBlogPostForm showModel post tags
            else do
              Log.logInfo "User not authorized to edit this blog post" (User.mId user, post.id)
              renderTemplate hxRequest (Just userMetadata) permissionDeniedTemplate
