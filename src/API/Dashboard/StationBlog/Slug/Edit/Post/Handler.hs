{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.Slug.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Post.Route (BlogEditForm (..))
import API.Links (apiLinks, dashboardStationBlogLinks, userLinks)
import API.Types (DashboardStationBlogRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadBlogHeroImage)
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

stationBlogListUrl :: Links.URI
stationBlogListUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

stationBlogEditGetUrl :: BlogPosts.Id -> Slug -> Text
stationBlogEditGetUrl pid slug =
  let uri = Links.linkURI $ dashboardStationBlogLinks.editGet pid slug
   in [i|/#{uri}|]

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
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  BlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer blogPostId slug cookie editForm = do
  let editUrl = stationBlogEditGetUrl blogPostId slug
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" blogPostId
      let banner = BannerParams Error "Login Required" "You must be logged in to edit blog posts."
      pure (Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner))
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          Log.logInfo "Non-staff user tried to edit station blog post" blogPostId
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to edit station blog posts."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
    Just (_user, _userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
        oldTags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
        MaybeT $ pure $ Just (blogPost, oldTags)

      case mResult of
        Left err -> do
          Log.logAttention "getBlogPostById execution error" (show err)
          let banner = BannerParams Warning "Blog Post Not Found" "The blog post you're trying to update doesn't exist."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{stationBlogListUrl}|] banner)
        Right Nothing -> do
          Log.logInfo "No blog post found with id" blogPostId
          let banner = BannerParams Warning "Blog Post Not Found" "The blog post you're trying to update doesn't exist."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{stationBlogListUrl}|] banner)
        Right (Just (blogPost, oldTags)) ->
          updateBlogPost editUrl blogPost oldTags editForm

updateBlogPost ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m
  ) =>
  Text ->
  BlogPosts.Model ->
  [BlogTags.Model] ->
  BlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateBlogPost editUrl blogPost oldTags editForm = do
  case parseStatus (befStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in blog edit form" (befStatus editForm)
      let banner = BannerParams Error "Update Failed" "Invalid blog post status value."
      pure $ Servant.noHeader (redirectWithBanner editUrl banner)
    Just parsedStatus -> do
      -- Process hero image upload if present
      heroImagePath <- case befHeroImage editForm of
        Nothing -> pure blogPost.bpmHeroImageUrl -- Keep existing image
        Just heroImageFile -> do
          let slug = Slug.mkSlug (befTitle editForm)
          uploadResult <- uploadBlogHeroImage slug heroImageFile
          case uploadResult of
            Left uploadError -> do
              Log.logInfo "Hero image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
              pure blogPost.bpmHeroImageUrl -- Keep existing image on error
            Right Nothing -> pure blogPost.bpmHeroImageUrl -- No file selected, keep existing
            Right (Just result) -> do
              Log.logInfo "Hero image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
              pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

      -- Sanitize user input to prevent XSS attacks
      let sanitizedTitle = Sanitize.sanitizeTitle (befTitle editForm)
          sanitizedContent = Sanitize.sanitizeUserContent (befContent editForm)
          sanitizedExcerpt = Sanitize.sanitizeDescription <$> befExcerpt editForm
          newSlug = Slug.mkSlug sanitizedTitle

      -- Validate content lengths
      case (Sanitize.validateContentLength 200 sanitizedTitle, Sanitize.validateContentLength 50000 sanitizedContent) of
        (Left titleError, _) -> do
          let errorMsg = Sanitize.displayContentValidationError titleError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (_, Left contentError) -> do
          let errorMsg = Sanitize.displayContentValidationError contentError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (Right validTitle, Right validContent) -> do
          let updateData =
                BlogPosts.Insert
                  { BlogPosts.bpiTitle = validTitle,
                    BlogPosts.bpiSlug = newSlug,
                    BlogPosts.bpiContent = validContent,
                    BlogPosts.bpiExcerpt = sanitizedExcerpt,
                    BlogPosts.bpiHeroImageUrl = heroImagePath,
                    BlogPosts.bpiAuthorId = blogPost.bpmAuthorId,
                    BlogPosts.bpiStatus = parsedStatus
                  }

          mUpdateResult <- execTransactionSpan $ runMaybeT $ do
            _ <- MaybeT $ HT.statement () (BlogPosts.updateBlogPost blogPost.bpmId updateData)
            lift $ traverse_ (\tag -> HT.statement () (BlogPosts.removeTagFromPost blogPost.bpmId tag.btmId)) oldTags
            lift $ updatePostTags blogPost.bpmId editForm
            MaybeT $ pure $ Just ()

          case mUpdateResult of
            Left err -> do
              Log.logInfo "Failed to update blog post" (blogPost.bpmId, show err)
              let banner = BannerParams Error "Update Failed" "Database error occurred. Please try again."
              pure $ Servant.noHeader (redirectWithBanner editUrl banner)
            Right Nothing -> do
              Log.logInfo "Blog post update returned Nothing" blogPost.bpmId
              let banner = BannerParams Error "Update Failed" "Failed to update blog post. Please try again."
              pure $ Servant.noHeader (redirectWithBanner editUrl banner)
            Right (Just _) -> do
              Log.logInfo "Successfully updated blog post" blogPost.bpmId
              let postId = blogPost.bpmId
                  detailLink = Links.linkURI $ dashboardStationBlogLinks.detail postId newSlug
                  detailUrl = [i|/#{detailLink}|] :: Text
                  banner = BannerParams Success "Blog Post Updated" "Your blog post has been updated and saved."
                  redirectUrl = buildRedirectUrl detailUrl banner
              pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)

-- | Update tags for a blog post (add new ones)
updatePostTags ::
  BlogPosts.Id ->
  BlogEditForm ->
  HT.Transaction ()
updatePostTags postId form = do
  let newTags = befTags form
  unless (null newTags) $
    traverse_ (createOrAssociateTag postId) newTags

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  BlogPosts.Id ->
  Text ->
  HT.Transaction ()
createOrAssociateTag postId tagName = do
  mExistingTag <- HT.statement () (BlogTags.getTagByName tagName)
  case mExistingTag of
    Just existingTag -> do
      -- If tag exists, associate it
      void $ HT.statement () (BlogPosts.addTagToPost postId (BlogTags.btmId existingTag))
    Nothing -> do
      -- Otherwise, create new tag and associate it
      newTagId <- HT.statement () (BlogTags.insertTag (BlogTags.Insert tagName))
      void $ HT.statement () (BlogPosts.addTagToPost postId newTagId)
