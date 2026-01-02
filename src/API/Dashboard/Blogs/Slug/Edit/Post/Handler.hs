{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Post.Route (ShowBlogEditForm (..))
import API.Links (dashboardBlogsLinks, rootLink)
import API.Types (DashboardBlogsRoutes (..))
import App.Handler.Combinators (requireAuth, requireHostNotSuspended, requireJust)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
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
import Servant qualified

--------------------------------------------------------------------------------

-- | Parse blog post status from text
parseStatus :: Text -> Maybe BlogPostStatus
parseStatus "published" = Just Published
parseStatus "draft" = Just Draft
parseStatus "deleted" = Just Deleted
parseStatus _ = Nothing

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
  ShowBlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug postId cookie editForm =
  handleRedirectErrors "Blog post update" (dashboardBlogsLinks.editGet showSlug postId) $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to edit blog posts." userMetadata

    -- 2. Fetch blog post, show, tags, and verify host permissions in a transaction
    (blogPost, showModel, oldTags, isHost) <- fetchBlogPostWithContext user userMetadata showSlug postId

    -- 3. Verify user is authorized to edit this post
    if not (blogPost.authorId == User.mId user || isHost)
      then throwNotAuthorized "You are not authorized to edit this blog post."
      else updateBlogPost showSlug postId showModel blogPost oldTags editForm

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

updateBlogPost ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Slug ->
  ShowBlogPosts.Id ->
  Shows.Model ->
  ShowBlogPosts.Model ->
  [ShowBlogTags.Model] ->
  ShowBlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateBlogPost _showSlug _postId showModel blogPost oldTags editForm = do
  -- 4. Validate status
  parsedStatus <- requireJust "Invalid blog post status value." (parseStatus (sbefStatus editForm))

  -- 5. Build update data
  let newSlug = Slug.mkSlug (sbefTitle editForm)
      updateData =
        ShowBlogPosts.Insert
          { ShowBlogPosts.sbpiId = blogPost.showId,
            ShowBlogPosts.sbpiTitle = sbefTitle editForm,
            ShowBlogPosts.sbpiSlug = newSlug,
            ShowBlogPosts.sbpiContent = sbefContent editForm,
            ShowBlogPosts.sbpiExcerpt = sbefExcerpt editForm,
            ShowBlogPosts.sbpiAuthorId = blogPost.authorId,
            ShowBlogPosts.sbpiStatus = parsedStatus
          }

  -- 6. Update blog post in a transaction
  mUpdateResult <- execTransactionSpan $ runMaybeT $ do
    void $ MaybeT $ HT.statement () (ShowBlogPosts.updateShowBlogPost blogPost.id updateData)
    lift $ traverse_ (HT.statement () . ShowBlogPosts.removeTagFromShowBlogPost blogPost.id . ShowBlogTags.sbtmId) oldTags
    lift $ updatePostTags blogPost.id editForm
    pure ()

  case mUpdateResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwDatabaseError (error "Blog post update returned Nothing")
    Right (Just _) -> do
      Log.logInfo "Successfully updated show blog post" blogPost.id
      let detailUrl = rootLink $ dashboardBlogsLinks.detail (Shows.slug showModel) blogPost.id
          banner = BannerParams Success "Blog Post Updated" "Your blog post has been updated successfully."
      pure $ Servant.addHeader (buildRedirectUrl detailUrl banner) (redirectWithBanner detailUrl banner)

-- | Update tags for a blog post (add new ones)
updatePostTags ::
  ShowBlogPosts.Id ->
  ShowBlogEditForm ->
  HT.Transaction ()
updatePostTags postId editForm =
  traverse_ (createOrAssociateTag postId) (sbefTags editForm)

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  ShowBlogPosts.Id ->
  Text ->
  HT.Transaction ()
createOrAssociateTag postId tagName =
  HT.statement () (ShowBlogTags.getShowBlogTagByName tagName) >>= \case
    Just existingTag -> do
      -- If tag exists, associate it
      HT.statement () (ShowBlogPosts.addTagToShowBlogPost postId (ShowBlogTags.sbtmId existingTag))
    Nothing -> do
      -- otherwise, create new tag and associate it
      newTagId <- HT.statement () (ShowBlogTags.insertShowBlogTag (ShowBlogTags.Insert tagName))
      HT.statement () (ShowBlogPosts.addTagToShowBlogPost postId newTagId)
