{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Post.Route (NewBlogPostForm (..))
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
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadBlogHeroImage)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardStationBlogNewGetUrl :: Links.URI
dashboardStationBlogNewGetUrl = Links.linkURI dashboardStationBlogLinks.newGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

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
  Maybe Cookie ->
  NewBlogPostForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to create blog posts."
      pure (Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner))
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to create blog posts."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
    Just (_user, userMetadata) -> do
      -- Process hero image upload if present
      heroImagePath <- case nbpfHeroImage form of
        Nothing -> pure Nothing
        Just heroImageFile -> do
          let slug = Slug.mkSlug (nbpfTitle form)
          uploadResult <- uploadBlogHeroImage slug heroImageFile
          case uploadResult of
            Left uploadError -> do
              Log.logInfo "Hero image upload failed" (Aeson.object ["error" .= Text.pack (show uploadError)])
              pure Nothing
            Right Nothing -> pure Nothing -- No file selected
            Right (Just result) -> do
              Log.logInfo "Hero image uploaded successfully" (Aeson.object ["path" .= uploadResultStoragePath result])
              pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

      case validateNewBlogPost form userMetadata.mUserId heroImagePath of
        Left validationError -> do
          let errorMsg = Sanitize.displayContentValidationError validationError
          Log.logInfo "Blog post creation failed" (Aeson.object ["message" .= errorMsg])
          let banner = BannerParams Error "Validation Error" errorMsg
          pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardStationBlogNewGetUrl}|] banner)
        Right blogPostData ->
          handlePostCreation blogPostData form

-- | Validate and convert form data to blog post insert data
validateNewBlogPost :: NewBlogPostForm -> User.Id -> Maybe Text -> Either Sanitize.ContentValidationError BlogPosts.Insert
validateNewBlogPost form authorId heroImagePath = do
  let status = fromMaybe Published $ decodeBlogPost =<< nbpfStatus form
      slug = Slug.mkSlug (nbpfTitle form)

      -- Sanitize user input to prevent XSS attacks
      sanitizedTitle = Sanitize.sanitizeTitle (nbpfTitle form)
      sanitizedContent = Sanitize.sanitizeUserContent (nbpfContent form)
      sanitizedExcerpt = Sanitize.sanitizeDescription <$> nbpfExcerpt form

  -- Validate sanitized content lengths
  validTitle <- Sanitize.validateContentLength 200 sanitizedTitle
  validContent <- Sanitize.validateContentLength 50000 sanitizedContent

  Right $
    BlogPosts.Insert
      { BlogPosts.bpiTitle = validTitle,
        BlogPosts.bpiSlug = slug,
        BlogPosts.bpiContent = validContent,
        BlogPosts.bpiExcerpt = sanitizedExcerpt,
        BlogPosts.bpiHeroImageUrl = heroImagePath,
        BlogPosts.bpiAuthorId = authorId,
        BlogPosts.bpiStatus = status
      }

-- | Create tags for a blog post
createPostTags ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  BlogPosts.Id ->
  NewBlogPostForm ->
  m ()
createPostTags postId form = do
  let tags = nbpfTags form
  unless (null tags) $
    traverse_ (createOrAssociateTag postId) tags

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  BlogPosts.Id ->
  Text ->
  m ()
createOrAssociateTag postId tagName =
  execQuerySpan (BlogTags.getTagByName tagName) >>= \case
    Right (Just existingTag) -> do
      -- If tag exists, associate it
      void $ execQuerySpan (BlogPosts.addTagToPost postId (BlogTags.btmId existingTag))
    _ -> do
      -- otherwise, create new tag and associate it
      tagInsertResult <- execQuerySpan (BlogTags.insertTag (BlogTags.Insert tagName))
      case tagInsertResult of
        Right newTagId -> do
          void $ execQuerySpan (BlogPosts.addTagToPost postId newTagId)
        Left dbError -> do
          Log.logInfo "Database error creating tag" (Aeson.object ["error" .= Text.pack (show dbError)])

-- | Handle blog post creation after validation passes
handlePostCreation ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  BlogPosts.Insert ->
  NewBlogPostForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlePostCreation blogPostData form = do
  execQuerySpan (BlogPosts.insertBlogPost blogPostData) >>= \case
    Left dbError -> do
      Log.logInfo "Database error creating blog post" (Aeson.object ["error" .= Text.pack (show dbError)])
      let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
      pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardStationBlogNewGetUrl}|] banner)
    Right postId -> do
      createPostTags postId form
      -- Fetch created post to get the slug
      execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
        Right (Just createdPost) -> do
          Log.logInfo "Successfully created blog post" (Aeson.object ["title" .= BlogPosts.bpmTitle createdPost])
          let createdSlug = BlogPosts.bpmSlug createdPost
              detailLink = Links.linkURI $ dashboardStationBlogLinks.detail postId createdSlug
              detailUrl = [i|/#{detailLink}|] :: Text
              banner = BannerParams Success "Blog Post Created" "Your blog post has been created successfully."
              redirectUrl = buildRedirectUrl detailUrl banner
          pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
        _ -> do
          Log.logInfo_ "Created blog post but failed to retrieve it"
          let banner = BannerParams Error "Error" "Post was created but there was an error displaying the confirmation."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardStationBlogGetUrl}|] banner)
