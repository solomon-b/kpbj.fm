{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogNewGetLink, blogPostGetLink, userLoginGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (fold, traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogNewGetUrl :: Links.URI
blogNewGetUrl = Links.linkURI blogNewGetLink

blogPostGetUrl :: Slug -> Links.URI
blogPostGetUrl slug = Links.linkURI $ blogPostGetLink slug

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
        :> Servant.ReqBody '[Servant.FormUrlEncoded] NewBlogPostForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Success template after blog creation
successTemplate :: BlogPosts.Model -> Lucid.Html ()
successTemplate post = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Blog Post Created Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] $ do
      "Your post \""
      Lucid.strong_ $ Lucid.toHtml (BlogPosts.bpmTitle post)
      "\" has been "
      case BlogPosts.bpmStatus post of
        Published -> "published and is now live."
        Draft -> "saved as a draft."
        Archived -> "archived."

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "VIEW POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetUrl}|],
          hxGet_ [i|/#{blogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "BACK TO BLOG"
      Lucid.a_
        [ Lucid.href_ [i|/#{blogNewGetUrl}|],
          hxGet_ [i|/#{blogNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
        ]
        "CREATE ANOTHER"

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

-- | Template for general user metadata error
userMetadataErrorTemplate :: Lucid.Html ()
userMetadataErrorTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ "Unable to load user information."

--------------------------------------------------------------------------------

-- | Form data for creating a new blog post
data NewBlogPostForm = NewBlogPostForm
  { nbpfTitle :: Text,
    nbpfContent :: Text,
    nbpfExcerpt :: Maybe Text,
    nbpfStatus :: Maybe Text,
    nbpfTags :: [Text]
  }
  deriving (Show, Eq)

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
          nbpfTags = parseTags $ fold tags
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
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing ->
      renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (_user, userMetadata) ->
      case UserMetadata.mUserRole userMetadata of
        role | UserMetadata.isStaffOrHigher role ->
          case validateNewBlogPost form (UserMetadata.mUserId userMetadata) of
            Left errorMsg -> do
              Log.logInfo ("Blog post creation failed: " <> errorMsg) ()
              renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
            Right blogPostData -> do
              template <- handlePostCreation blogPostData form
              renderTemplate hxRequest (Just userMetadata) template
        _ ->
          renderTemplate hxRequest Nothing permissionDeniedTemplate

-- | Validate and convert form data to blog post insert data
validateNewBlogPost :: NewBlogPostForm -> User.Id -> Either Text BlogPosts.Insert
validateNewBlogPost form authorId = do
  when (Text.null (nbpfTitle form)) (Left "Title is required")
  when (Text.null (nbpfContent form)) (Left "Content is required")

  let status = fromMaybe Published $ decodeBlogPost =<< nbpfStatus form
      slug = Slug.mkSlug (nbpfTitle form)

  Right $
    BlogPosts.Insert
      { BlogPosts.bpiTitle = nbpfTitle form,
        BlogPosts.bpiSlug = slug,
        BlogPosts.bpiContent = nbpfContent form,
        BlogPosts.bpiExcerpt = nbpfExcerpt form,
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
          Log.logInfo ("Database error creating tag: " <> Text.pack (show dbError)) ()
          pure ()

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
  m (Lucid.Html ())
handlePostCreation blogPostData form = do
  execQuerySpan (BlogPosts.insertBlogPost blogPostData) >>= \case
    Left dbError -> do
      Log.logInfo ("Database error creating blog post: " <> Text.pack (show dbError)) ()
      pure (errorTemplate "Database error occurred. Please try again.")
    Right postId -> do
      execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
        Right (Just createdPost) -> do
          createPostTags postId form
          Log.logInfo ("Successfully created blog post: " <> BlogPosts.bpmTitle createdPost) ()
          pure (successTemplate createdPost)
        _ -> do
          Log.logInfo "Created blog post but failed to retrieve it" ()
          pure (errorTemplate "Post was created but there was an error displaying the confirmation.")
