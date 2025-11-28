{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogNewGetLink, blogPostGetLink, userLoginGetLink)
import API.Blog.Post.Get.Templates.Page qualified as DetailPage
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (fold, traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadBlogHeroImage)
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogNewGetUrl :: Links.URI
blogNewGetUrl = Links.linkURI blogNewGetLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /blog/new"
    ( "blog"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem NewBlogPostForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Error template
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "❌ Error Creating Blog Post"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogNewGetUrl}|],
          hxGet_ [i|/#{blogNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "TRY AGAIN"
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetUrl}|],
          hxGet_ [i|/#{blogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "BACK TO BLOG"

-- | Template for login required error
loginRequiredTemplate :: Lucid.Html ()
loginRequiredTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-4"] "You must be logged in to create blog posts."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "LOGIN"

-- | Template for permission denied error
permissionDeniedTemplate :: Lucid.Html ()
permissionDeniedTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ [Lucid.class_ "mb-4"] "Only Staff and Admin users can create blog posts."
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BACK TO BLOG"

--------------------------------------------------------------------------------

-- | Form data for creating a new blog post
data NewBlogPostForm = NewBlogPostForm
  { nbpfTitle :: Text,
    nbpfContent :: Text,
    nbpfExcerpt :: Maybe Text,
    nbpfStatus :: Maybe Text,
    nbpfTags :: [Text],
    nbpfHeroImage :: Maybe (FileData Mem)
  }
  deriving (Show, Eq)

instance FromMultipart Mem NewBlogPostForm where
  fromMultipart multipartData =
    NewBlogPostForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "content" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "excerpt" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "status" multipartData))
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "hero_image" multipartData))
    where
      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

instance FromForm NewBlogPostForm where
  fromForm form = do
    title <- parseUnique "title" form
    content <- parseUnique "content" form
    excerpt <- parseMaybe "excerpt" form
    status <- parseMaybe "status" form
    tags <- parseMaybe "tags" form

    pure
      NewBlogPostForm
        { nbpfTitle = title,
          nbpfContent = content,
          nbpfExcerpt = if maybe True Text.null excerpt then Nothing else excerpt,
          nbpfStatus = status,
          nbpfTags = parseTags $ fold tags,
          nbpfHeroImage = Nothing
        }

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map Text.strip $
      Text.splitOn "," tagText

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
  Maybe HxRequest ->
  NewBlogPostForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing ->
      Servant.noHeader <$> renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (_user, userMetadata) ->
      case userMetadata.mUserRole of
        role | UserMetadata.isStaffOrHigher role -> do
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
                Right result -> do
                  Log.logInfo "Hero image uploaded successfully" (Aeson.object ["path" .= uploadResultStoragePath result])
                  pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

          case validateNewBlogPost form userMetadata.mUserId heroImagePath of
            Left validationError -> do
              let errorMsg = Sanitize.displayContentValidationError validationError
              Log.logInfo "Blog post creation failed" (Aeson.object ["message" .= errorMsg])
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
            Right blogPostData ->
              handlePostCreation hxRequest userMetadata blogPostData form
        _ ->
          Servant.noHeader <$> renderTemplate hxRequest Nothing permissionDeniedTemplate

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
  HxRequest ->
  UserMetadata.Model ->
  BlogPosts.Insert ->
  NewBlogPostForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handlePostCreation hxRequest userMetadata blogPostData form = do
  execQuerySpan (BlogPosts.insertBlogPost blogPostData) >>= \case
    Left dbError -> do
      Log.logInfo "Database error creating blog post" (Aeson.object ["error" .= Text.pack (show dbError)])
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
    Right postId -> do
      createPostTags postId form
      -- Fetch created post with author and tags for detail page
      execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
        Right (Just createdPost) -> do
          -- Get tags for the post
          tagsResult <- execQuerySpan (BlogPosts.getTagsForPost postId)
          let tags = either (const []) id tagsResult
          Log.logInfo "Successfully created blog post" (Aeson.object ["title" .= BlogPosts.bpmTitle createdPost])
          let detailUrl = Links.linkURI $ blogPostGetLink postId (BlogPosts.bpmSlug createdPost)
              banner = renderBanner Success "Blog Post Created" "Your blog post has been created successfully."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> do
              DetailPage.template createdPost userMetadata tags
              banner
            IsNotHxRequest -> do
              banner
              DetailPage.template createdPost userMetadata tags
          pure $ Servant.addHeader [i|/#{detailUrl}|] html
        _ -> do
          Log.logInfo_ "Created blog post but failed to retrieve it"
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Post was created but there was an error displaying the confirmation.")
