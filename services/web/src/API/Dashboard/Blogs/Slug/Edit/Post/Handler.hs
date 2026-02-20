{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Post.Route (ShowBlogEditForm (..))
import API.Links (dashboardBlogsLinks, rootLink)
import API.Types (DashboardBlogsRoutes (..))
import App.Handler.Combinators (requireAuth, requireHostNotSuspended, requireJust)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Parse blog post status from text
parseStatus :: Text -> Maybe BlogPostStatus
parseStatus "published" = Just Published
parseStatus "draft" = Just Draft
parseStatus "deleted" = Just Deleted
parseStatus _ = Nothing

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + redirect response.
handler ::
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  ShowBlogEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showSlug postId cookie editForm =
  handleRedirectErrors "Blog post update" (dashboardBlogsLinks.editGet showSlug postId) $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to edit blog posts." userMetadata
    (showModel, updatedPostId) <- action user userMetadata showSlug postId editForm
    let detailUrl = rootLink $ dashboardBlogsLinks.detail (Shows.slug showModel) updatedPostId
        banner = BannerParams Success "Blog Post Updated" "Your blog post has been updated successfully."
    pure $ Servant.addHeader (buildRedirectUrl detailUrl banner) (redirectWithBanner detailUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: post context fetch, authorization check, update, tag sync.
-- Returns the show model and post ID needed to build the redirect URL.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  ShowBlogEditForm ->
  ExceptT HandlerError AppM (Shows.Model, ShowBlogPosts.Id)
action user userMetadata showSlug postId editForm = do
  -- 1. Fetch blog post, show, tags, and verify host permissions in a transaction
  (blogPost, showModel, oldTags, isHost) <- fetchBlogPostWithContext user userMetadata showSlug postId

  -- 2. Verify user is authorized to edit this post
  if not (blogPost.authorId == User.mId user || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole)
    then throwNotAuthorized "You are not authorized to edit this blog post." (Just userMetadata.mUserRole)
    else do
      -- 3. Validate status
      parsedStatus <- requireJust "Invalid blog post status value." (parseStatus (sbefStatus editForm))

      -- 4. Build update data
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

      -- 5. Update blog post in a transaction
      mUpdateResult <-
        fromRightM throwDatabaseError $
          execTransaction $
            runMaybeT $ do
              void $ MaybeT $ HT.statement () (ShowBlogPosts.updateShowBlogPost blogPost.id updateData)
              lift $ traverse_ (HT.statement () . ShowBlogPosts.removeTagFromShowBlogPost blogPost.id . ShowBlogTags.sbtmId) oldTags
              lift $ updatePostTags blogPost.id editForm
              pure ()

      case mUpdateResult of
        Nothing -> throwHandlerFailure "Blog post update returned Nothing"
        Just _ -> do
          Log.logInfo "Successfully updated show blog post" blogPost.id
          pure (showModel, blogPost.id)

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
      mNewTagId <- HT.statement () (ShowBlogTags.insertShowBlogTag (ShowBlogTags.Insert tagName))
      case mNewTagId of
        Just newTagId -> HT.statement () (ShowBlogPosts.addTagToShowBlogPost postId newTagId)
        Nothing -> pure () -- Insert failed unexpectedly, skip this tag
