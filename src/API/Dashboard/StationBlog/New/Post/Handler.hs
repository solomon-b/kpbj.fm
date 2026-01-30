module API.Dashboard.StationBlog.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Post.Route (NewBlogPostForm (..))
import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.FileUpload (uploadBlogHeroImage)
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  NewBlogPostForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Blog post creation" dashboardStationBlogLinks.newGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create blog posts." userMetadata

    -- 2. Process hero image upload if present
    heroImagePath <- handleHeroImageUpload form

    -- 3. Validate and create blog post
    blogPostData <-
      requireRight
        Sanitize.displayContentValidationError
        (validateNewBlogPost form (User.mId user) heroImagePath)
    handlePostCreation blogPostData form

-- | Handle hero image upload
handleHeroImageUpload ::
  NewBlogPostForm ->
  AppM (Maybe Text)
handleHeroImageUpload form = case nbpfHeroImage form of
  Nothing -> pure Nothing
  Just heroImageFile -> do
    let slug = Slug.mkSlug (nbpfTitle form)
    storageBackend <- asks getter
    mAwsEnv <- asks getter
    uploadResult <- uploadBlogHeroImage storageBackend mAwsEnv slug heroImageFile
    case uploadResult of
      Left uploadError -> do
        Log.logInfo "Hero image upload failed" (Aeson.object ["error" .= Text.pack (show uploadError)])
        pure Nothing
      Right Nothing -> pure Nothing
      Right (Just result) -> do
        Log.logInfo "Hero image uploaded successfully" (Aeson.object ["path" .= uploadResultStoragePath result])
        pure (Just $ Text.pack $ uploadResultStoragePath result)

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
  BlogPosts.Id ->
  NewBlogPostForm ->
  AppM ()
createPostTags postId form = do
  let tags = nbpfTags form
  unless (null tags) $
    traverse_ (createOrAssociateTag postId) tags

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  BlogPosts.Id ->
  Text ->
  AppM ()
createOrAssociateTag postId tagName =
  execQuery (BlogTags.getTagByName tagName) >>= \case
    Right (Just existingTag) -> do
      -- If tag exists, associate it
      void $ execQuery (BlogPosts.addTagToPost postId (BlogTags.btmId existingTag))
    _ -> do
      -- otherwise, create new tag and associate it
      tagInsertResult <- execQuery (BlogTags.insertTag (BlogTags.Insert tagName))
      case tagInsertResult of
        Right newTagId -> do
          void $ execQuery (BlogPosts.addTagToPost postId newTagId)
        Left dbError -> do
          Log.logInfo "Database error creating tag" (Aeson.object ["error" .= Text.pack (show dbError)])

-- | Handle blog post creation after validation passes
handlePostCreation ::
  BlogPosts.Insert ->
  NewBlogPostForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlePostCreation blogPostData form = do
  postId <- insertBlogPost blogPostData
  createPostTags postId form

  -- Fetch created post to get the slug
  execQuery (BlogPosts.getBlogPostById postId) >>= \case
    Right (Just createdPost) -> do
      Log.logInfo "Successfully created blog post" (Aeson.object ["title" .= BlogPosts.bpmTitle createdPost])
      let createdSlug = BlogPosts.bpmSlug createdPost
          detailUrl = rootLink $ dashboardStationBlogLinks.detail postId createdSlug
          banner = BannerParams Success "Blog Post Created" "Your blog post has been created successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
    _ -> do
      Log.logInfo_ "Created blog post but failed to retrieve it"
      let listUrl = rootLink $ dashboardStationBlogLinks.list Nothing
          banner = BannerParams Success "Blog Post Created" "Your blog post has been created."
          redirectUrl = buildRedirectUrl listUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

insertBlogPost ::
  BlogPosts.Insert ->
  AppM BlogPosts.Id
insertBlogPost blogPostData =
  execQuery (BlogPosts.insertBlogPost blogPostData) >>= \case
    Left err -> throwDatabaseError err
    Right postId -> pure postId
