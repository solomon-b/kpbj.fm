{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationBlog.Slug.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Post.Route (BlogEditForm (..))
import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireJust, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Has (getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.PostStatus (decodeBlogPost)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.FileUpload (uploadBlogHeroImage)
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Data returned by action after successful blog post update.
data EditBlogPostResult = EditBlogPostResult
  { ebprRedirectUrl :: Text,
    ebprBanner :: BannerParams
  }

-- | Business logic: fetch, validate, update post, return redirect info.
action ::
  BlogPosts.Id ->
  Slug ->
  BlogEditForm ->
  ExceptT HandlerError AppM EditBlogPostResult
action blogPostId _slug editForm = do
  -- 1. Fetch blog post and old tags
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
          oldTags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
          pure (blogPost, oldTags)

  (blogPost, oldTags) <- case mResult of
    Nothing -> throwNotFound "Blog post"
    Just result -> pure result

  -- 2. Update the blog post
  updateBlogPost blogPost oldTags editForm

-- | Servant handler: thin glue composing action + return redirect response.
handler ::
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  BlogEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler blogPostId slug cookie editForm =
  handleRedirectErrors "Station blog update" (dashboardStationBlogLinks.editGet blogPostId slug) $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit station blog posts." userMetadata
    result <- action blogPostId slug editForm
    pure $ Servant.addHeader result.ebprRedirectUrl (redirectWithBanner result.ebprRedirectUrl result.ebprBanner)

updateBlogPost ::
  BlogPosts.Model ->
  [BlogTags.Model] ->
  BlogEditForm ->
  ExceptT HandlerError AppM EditBlogPostResult
updateBlogPost blogPost oldTags editForm = do
  -- 4. Validate status
  parsedStatus <- requireJust "Invalid blog post status value." (decodeBlogPost (befStatus editForm))

  -- 5. Process hero image upload if present, or clear if requested
  heroImagePath <- case befHeroImage editForm of
    Nothing ->
      -- No new file: check if user wants to clear existing image
      if befHeroImageClear editForm
        then do
          Log.logInfo "Clearing hero image" blogPost.bpmId
          pure Nothing
        else pure blogPost.bpmHeroImageUrl -- Keep existing image
    Just heroImageFile -> do
      let slug = Slug.mkSlug (befTitle editForm)
      storageBackend <- asks getter
      mAwsEnv <- asks getter
      uploadResult <- lift $ uploadBlogHeroImage storageBackend mAwsEnv slug heroImageFile
      case uploadResult of
        Left uploadError -> do
          Log.logInfo "Hero image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
          pure blogPost.bpmHeroImageUrl -- Keep existing image on error
        Right Nothing ->
          -- No file selected: check if user wants to clear existing image
          if befHeroImageClear editForm
            then do
              Log.logInfo "Clearing hero image" blogPost.bpmId
              pure Nothing
            else pure blogPost.bpmHeroImageUrl -- Keep existing
        Right (Just result) -> do
          Log.logInfo "Hero image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
          pure (Just $ Text.pack $ uploadResultStoragePath result)

  -- 6. Sanitize and validate content
  let sanitizedTitle = Sanitize.sanitizeTitle (befTitle editForm)
      sanitizedContent = Sanitize.sanitizeUserContent (befContent editForm)
      sanitizedExcerpt = Sanitize.sanitizeDescription <$> befExcerpt editForm
      newSlug = Slug.mkSlug sanitizedTitle

  validTitle <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 200 sanitizedTitle)
  validContent <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 50000 sanitizedContent)

  -- 7. Build update data
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

  -- 8. Update in transaction
  mUpdateResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          _ <- MaybeT $ HT.statement () (BlogPosts.updateBlogPost blogPost.bpmId updateData)
          lift $ traverse_ (\tag -> HT.statement () (BlogPosts.removeTagFromPost blogPost.bpmId tag.btmId)) oldTags
          lift $ updatePostTags blogPost.bpmId editForm
          pure ()

  case mUpdateResult of
    Nothing -> throwHandlerFailure "Blog post update returned Nothing"
    Just _ -> do
      Log.logInfo "Successfully updated blog post" blogPost.bpmId
      let detailUrl = rootLink $ dashboardStationBlogLinks.detail blogPost.bpmId newSlug
          banner = BannerParams Success "Blog Post Updated" "Your blog post has been updated and saved."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ EditBlogPostResult redirectUrl banner

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
      mNewTagId <- HT.statement () (BlogTags.insertTag (BlogTags.Insert tagName))
      case mNewTagId of
        Just newTagId -> void $ HT.statement () (BlogPosts.addTagToPost postId newTagId)
        Nothing -> pure () -- Insert failed unexpectedly, skip this tag
