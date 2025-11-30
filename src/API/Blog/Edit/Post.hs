{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogPostGetLink)
import API.Blog.Post.Get.Templates.Page qualified as DetailPage
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Foldable (fold, traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadBlogHeroImage)
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /blog/:id/:slug/edit"
    ( "blog"
        :> Servant.Capture "id" BlogPosts.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem BlogEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Form data for blog post editing
data BlogEditForm = BlogEditForm
  { befTitle :: Text,
    befContent :: Text,
    befExcerpt :: Maybe Text,
    befStatus :: Text,
    befTags :: [Text],
    befHeroImage :: Maybe (FileData Mem)
  }
  deriving (Show)

instance FromMultipart Mem BlogEditForm where
  fromMultipart multipartData =
    BlogEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "content" multipartData
      <*> pure (emptyToNothing $ either (const Nothing) Just (lookupInput "excerpt" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "hero_image" multipartData))
    where
      -- \| Convert empty text to Nothing
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

instance FromForm BlogEditForm where
  fromForm :: Form.Form -> Either Text BlogEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    content <- Form.parseUnique "content" form
    excerpt <- Form.parseMaybe "excerpt" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form

    pure
      BlogEditForm
        { befTitle = title,
          befContent = content,
          befExcerpt = emptyToNothing excerpt,
          befStatus = status,
          befTags = parseTags $ fromMaybe "" tags,
          befHeroImage = Nothing
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse comma-separated tags with sanitization
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText

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
  Maybe HxRequest ->
  BlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer blogPostId _slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" blogPostId
      Servant.noHeader <$> renderTemplate hxRequest Nothing (renderBanner Error "Access Denied" "You must be logged in to edit blog posts.")
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
        oldTags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
        MaybeT $ pure $ Just (blogPost, oldTags)

      case mResult of
        Left err -> do
          Log.logAttention "getBlogPostById execution error" (show err)
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Warning "Blog Post Not Found" "The blog post you're trying to update doesn't exist.")
        Right Nothing -> do
          Log.logInfo "No blog post found with id" blogPostId
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Warning "Blog Post Not Found" "The blog post you're trying to update doesn't exist.")
        Right (Just (blogPost, oldTags)) ->
          if blogPost.bpmAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then updateBlogPost hxRequest userMetadata blogPost oldTags editForm
            else do
              Log.logInfo "User attempted to edit blog post they don't own" blogPost.bpmId
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Access Denied" "You can only edit blog posts you authored or have staff permissions.")

updateBlogPost ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  BlogPosts.Model ->
  [BlogTags.Model] ->
  BlogEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
updateBlogPost hxRequest userMetadata blogPost oldTags editForm = do
  case parseStatus (befStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in blog edit form" (befStatus editForm)
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" "Invalid blog post status value.")
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
            Right result -> do
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
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" errorMsg)
        (_, Left contentError) -> do
          let errorMsg = Sanitize.displayContentValidationError contentError
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" errorMsg)
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
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" "Database error occurred. Please try again.")
            Right Nothing -> do
              Log.logInfo "Blog post update returned Nothing" blogPost.bpmId
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" "Failed to update blog post. Please try again.")
            Right (Just _) -> do
              Log.logInfo "Successfully updated blog post" blogPost.bpmId
              -- Fetch updated post data for detail page
              updatedPostData <- execTransactionSpan $ runMaybeT $ do
                updatedPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPost.bpmId)
                author <- MaybeT $ HT.statement () (UserMetadata.getUserMetadata updatedPost.bpmAuthorId)
                newTags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
                pure (updatedPost, author, newTags)

              case updatedPostData of
                Left _err ->
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" "Post updated but failed to load details.")
                Right Nothing ->
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (renderBanner Error "Update Failed" "Post updated but not found.")
                Right (Just (updatedPost, author, newTags)) -> do
                  let detailUrl = Links.linkURI $ blogPostGetLink blogPost.bpmId newSlug
                      banner = renderBanner Success "Blog Post Updated" "Your blog post has been updated and saved."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> do
                      -- HTMX request: render content first, banner uses OOB swap
                      DetailPage.template updatedPost author newTags
                      banner
                    IsNotHxRequest -> do
                      -- Regular request: render banner first (no OOB swap available)
                      banner
                      DetailPage.template updatedPost author newTags
                  pure $ Servant.addHeader [i|/#{detailUrl}|] html

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
