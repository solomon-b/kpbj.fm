module API.Dashboard.StationBlog.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Post.Route (NewBlogPostForm (..))
import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
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
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Data returned by action after successful blog post creation.
data NewBlogPostResult = NewBlogPostResult
  { nbprRedirectUrl :: Text,
    nbprBanner :: BannerParams
  }

-- | Business logic: validate, create post, return redirect info.
action ::
  User.Model ->
  NewBlogPostForm ->
  ExceptT HandlerError AppM NewBlogPostResult
action user form = do
  -- 1. Process hero image upload if present
  heroImagePath <- lift $ handleHeroImageUpload form

  -- 2. Validate and create blog post
  blogPostData <-
    requireRight
      Sanitize.displayContentValidationError
      (validateNewBlogPost form (User.mId user) heroImagePath)

  -- 3. Check slug uniqueness
  existingPost <- fromRightM throwDatabaseError $ execQuery (BlogPosts.getBlogPostBySlug blogPostData.bpiSlug)
  case existingPost of
    Just _ -> throwValidationError "A blog post with this URL already exists. Try a different title."
    Nothing -> pure ()

  buildNewPostResult blogPostData form

-- | Servant handler: thin glue composing action + return redirect response.
handler ::
  Maybe Cookie ->
  NewBlogPostForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Blog post creation" dashboardStationBlogLinks.newGet $ do
    (user, _userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create blog posts." _userMetadata
    result <- action user form
    pure $ Servant.addHeader result.nbprRedirectUrl (redirectWithBanner result.nbprRedirectUrl result.nbprBanner)

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
        Right (Just newTagId) -> do
          void $ execQuery (BlogPosts.addTagToPost postId newTagId)
        Right Nothing -> do
          Log.logInfo "Tag insert returned Nothing" (Aeson.object ["tagName" .= tagName])
        Left dbError -> do
          Log.logInfo "Database error creating tag" (Aeson.object ["error" .= Text.pack (show dbError)])

-- | Build result after blog post creation.
buildNewPostResult ::
  BlogPosts.Insert ->
  NewBlogPostForm ->
  ExceptT HandlerError AppM NewBlogPostResult
buildNewPostResult blogPostData form = do
  postId <- insertBlogPost blogPostData
  lift $ createPostTags postId form

  -- Fetch created post to get the slug
  execQuery (BlogPosts.getBlogPostById postId) >>= \case
    Right (Just createdPost) -> do
      Log.logInfo "Successfully created blog post" (Aeson.object ["title" .= BlogPosts.bpmTitle createdPost])
      let createdSlug = BlogPosts.bpmSlug createdPost
          detailUrl = rootLink $ dashboardStationBlogLinks.detail postId createdSlug
          banner = BannerParams Success "Blog Post Created" "Your blog post has been created successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ NewBlogPostResult redirectUrl banner
    _ -> do
      Log.logInfo_ "Created blog post but failed to retrieve it"
      let listUrl = rootLink $ dashboardStationBlogLinks.list Nothing
          banner = BannerParams Success "Blog Post Created" "Your blog post has been created."
          redirectUrl = buildRedirectUrl listUrl banner
      pure $ NewBlogPostResult redirectUrl banner

insertBlogPost ::
  BlogPosts.Insert ->
  ExceptT HandlerError AppM BlogPosts.Id
insertBlogPost blogPostData =
  fromMaybeM (throwHandlerFailure "Blog post insert returned Nothing") $
    fromRightM throwDatabaseError $
      execQuery (BlogPosts.insertBlogPost blogPostData)
