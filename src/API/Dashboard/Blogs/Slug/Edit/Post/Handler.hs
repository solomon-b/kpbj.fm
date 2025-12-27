{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Post.Route (ShowBlogEditForm (..))
import API.Links (apiLinks, dashboardBlogsLinks, userLinks)
import API.Types (DashboardBlogsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.String.Interpolate (i)
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
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

userLoginGetUrlText :: Text
userLoginGetUrlText = [i|/#{userLoginGetUrl}|]

dashboardBlogsDetailUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogsDetailUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.detail showSlug postId

dashboardBlogsDetailUrlText :: Slug -> ShowBlogPosts.Id -> Text
dashboardBlogsDetailUrlText showSlug postId = [i|/#{dashboardBlogsDetailUrl showSlug postId}|]

dashboardBlogsEditGetUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogsEditGetUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.editGet showSlug postId

dashboardBlogsEditGetUrlText :: Slug -> ShowBlogPosts.Id -> Text
dashboardBlogsEditGetUrlText showSlug postId = [i|/#{dashboardBlogsEditGetUrl showSlug postId}|]

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

rootGetUrlText :: Text
rootGetUrlText = [i|/#{rootGetUrl}|]

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
handler _tracer showSlug postId cookie editForm = do
  let editUrl = dashboardBlogsEditGetUrlText showSlug postId
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" (showSlug, postId)
      let banner = BannerParams Error "Login Required" "You must be logged in to edit blog posts."
      pure $ Servant.addHeader (buildRedirectUrl userLoginGetUrlText banner) (redirectWithBanner userLoginGetUrlText banner)
    Just (_user, userMetadata)
      | UserMetadata.isSuspended userMetadata -> do
          let banner = BannerParams Error "Account Suspended" "Your account has been suspended. You cannot edit blog posts."
          pure $ Servant.addHeader (buildRedirectUrl rootGetUrlText banner) (redirectWithBanner rootGetUrlText banner)
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostById postId)
        showModel <- MaybeT $ HT.statement () (Shows.getShowById blogPost.showId)
        -- Verify the show slug matches
        MaybeT $ pure $ if showModel.slug == showSlug then Just () else Nothing
        oldTags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) blogPost.showId)
        MaybeT $ pure $ Just (blogPost, showModel, oldTags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getShowBlogPostById execution error" (show err)
          let banner = BannerParams Error "Blog Post Not Found" "The blog post you're trying to edit does not exist."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right Nothing -> do
          Log.logInfo "No blog post found" (showSlug, postId)
          let banner = BannerParams Error "Blog Post Not Found" "The blog post you're trying to edit does not exist."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right (Just (blogPost, showModel, oldTags, isHost)) ->
          if blogPost.authorId == User.mId user || isHost
            then updateBlogPost showSlug postId showModel blogPost oldTags editForm
            else do
              Log.logInfo "User attempted to edit blog post they're not authorized for" blogPost.id
              let banner = BannerParams Error "Permission Denied" "You are not authorized to edit this blog post."
              pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)

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
updateBlogPost showSlug postId showModel blogPost oldTags editForm = do
  let editUrl = dashboardBlogsEditGetUrlText showSlug postId
  case parseStatus (sbefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in blog edit form" (sbefStatus editForm)
      let banner = BannerParams Error "Invalid Status" "Invalid blog post status value."
      pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
    Just parsedStatus -> do
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

      mUpdateResult <- execTransactionSpan $ runMaybeT $ do
        void $ MaybeT $ HT.statement () (ShowBlogPosts.updateShowBlogPost blogPost.id updateData)
        lift $ traverse_ (HT.statement () . ShowBlogPosts.removeTagFromShowBlogPost blogPost.id . ShowBlogTags.sbtmId) oldTags
        lift $ updatePostTags blogPost.id editForm
        MaybeT $ pure $ Just ()

      case mUpdateResult of
        Left err -> do
          Log.logInfo "Failed to update show blog post" (blogPost.id, show err)
          let banner = BannerParams Error "Update Failed" "Database error occurred. Please try again."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right Nothing -> do
          Log.logInfo "Blog post update returned Nothing" blogPost.id
          let banner = BannerParams Error "Update Failed" "Failed to update blog post. Please try again."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right (Just _) -> do
          Log.logInfo "Successfully updated show blog post" blogPost.id
          let detailUrl = dashboardBlogsDetailUrlText (Shows.slug showModel) blogPost.id
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
