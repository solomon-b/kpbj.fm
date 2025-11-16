{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogGetLink, showBlogNewGetLink, showBlogPostGetLink, showGetLink, userLoginGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (guard, unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
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
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
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
showGetUrl :: Slug -> Links.URI
showGetUrl showSlug = Links.linkURI $ showGetLink showSlug

showBlogGetUrl :: Slug -> Links.URI
showBlogGetUrl showSlug = Links.linkURI $ showBlogGetLink showSlug Nothing Nothing

showBlogNewGetUrl :: Slug -> Links.URI
showBlogNewGetUrl showSlug = Links.linkURI $ showBlogNewGetLink showSlug

showBlogPostGetUrl :: Slug -> Slug -> Links.URI
showBlogPostGetUrl showSlug postSlug = Links.linkURI $ showBlogPostGetLink showSlug postSlug

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/blog/new"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "blog"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] NewShowBlogPostForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Success template after blog creation
successTemplate :: Slug -> ShowBlogPosts.Model -> Lucid.Html ()
successTemplate showSlug post = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Blog Post Created Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] $ do
      "Your post \""
      Lucid.strong_ $ Lucid.toHtml (ShowBlogPosts.title post)
      "\" has been "
      case ShowBlogPosts.status post of
        Published -> "published and is now live."
        Draft -> "saved as a draft."
        Archived -> "archived."

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogPostGetUrl showSlug (ShowBlogPosts.slug post)}|],
          hxGet_ [i|/#{showBlogPostGetUrl showSlug (ShowBlogPosts.slug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "VIEW POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "BACK TO BLOG"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogNewGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogNewGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
        ]
        "CREATE ANOTHER"

-- | Error template
errorTemplate :: Slug -> Text -> Lucid.Html ()
errorTemplate showSlug errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "❌ Error Creating Blog Post"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogNewGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogNewGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "TRY AGAIN"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogGetUrl showSlug}|],
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
permissionDeniedTemplate :: Slug -> Lucid.Html ()
permissionDeniedTemplate showSlug =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ [Lucid.class_ "mb-4"] "You must be a host of this show to create blog posts."
    Lucid.a_
      [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
        hxGet_ [i|/#{showGetUrl showSlug}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BACK TO SHOW"

--------------------------------------------------------------------------------

-- | Form data for creating a new show blog post
data NewShowBlogPostForm = NewShowBlogPostForm
  { nsbpfTitle :: Text,
    nsbpfContent :: Text,
    nsbpfExcerpt :: Maybe Text,
    nsbpfStatus :: Maybe Text,
    nsbpfTags :: [Text]
  }
  deriving (Show, Eq)

instance FromForm NewShowBlogPostForm where
  fromForm form = do
    title <- parseUnique "title" form
    content <- parseUnique "content" form
    excerpt <- parseMaybe "excerpt" form
    status <- parseMaybe "status" form
    tags <- parseMaybe "tags" form

    pure
      NewShowBlogPostForm
        { nsbpfTitle = title,
          nsbpfContent = content,
          nsbpfExcerpt = if maybe True Text.null excerpt then Nothing else excerpt,
          nsbpfStatus = status,
          nsbpfTags = parseTags $ fold tags
        }

-- | Parse comma-separated tags with sanitization
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  NewShowBlogPostForm ->
  m (Lucid.Html ())
handler _tracer showSlug cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing ->
      renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (user, userMetadata) -> do
      -- Fetch show and verify host permissions in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
        isHost <- lift $ HT.statement () (Shows.isUserHostOfShow (User.mId user) showModel.id)
        guard isHost
        MaybeT $ pure $ Just showModel

      case mResult of
        Left err -> do
          Log.logAttention "Failed to process show blog post creation" (show err)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate showSlug "Failed to create blog post. Please try again."
        Right Nothing -> do
          Log.logInfo "Show not found or user not authorized" (showSlug, User.mId user)
          renderTemplate hxRequest (Just userMetadata) $ permissionDeniedTemplate showSlug
        Right (Just showModel) -> do
          case validateNewShowBlogPost form showModel.id (UserMetadata.mUserId userMetadata) of
            Left validationError -> do
              let errorMsg = Sanitize.displayContentValidationError validationError
              Log.logInfo ("Show blog post creation failed: " <> errorMsg) ()
              renderTemplate hxRequest (Just userMetadata) (errorTemplate showSlug errorMsg)
            Right blogPostData -> do
              template <- handlePostCreation blogPostData form showSlug
              renderTemplate hxRequest (Just userMetadata) template

-- | Validate and convert form data to blog post insert data
validateNewShowBlogPost :: NewShowBlogPostForm -> Shows.Id -> User.Id -> Either Sanitize.ContentValidationError ShowBlogPosts.Insert
validateNewShowBlogPost form showId authorId = do
  let status = fromMaybe Published $ decodeBlogPost =<< nsbpfStatus form
      slug = Slug.mkSlug (nsbpfTitle form)

      -- Sanitize user input to prevent XSS attacks
      sanitizedTitle = Sanitize.sanitizeTitle (nsbpfTitle form)
      sanitizedContent = Sanitize.sanitizeUserContent (nsbpfContent form)
      sanitizedExcerpt = Sanitize.sanitizeDescription <$> nsbpfExcerpt form

  -- Validate sanitized content lengths
  validTitle <- Sanitize.validateContentLength 200 sanitizedTitle
  validContent <- Sanitize.validateContentLength 50000 sanitizedContent

  Right $
    ShowBlogPosts.Insert
      { ShowBlogPosts.sbpiId = showId,
        ShowBlogPosts.sbpiTitle = validTitle,
        ShowBlogPosts.sbpiSlug = slug,
        ShowBlogPosts.sbpiContent = validContent,
        ShowBlogPosts.sbpiExcerpt = sanitizedExcerpt,
        ShowBlogPosts.sbpiAuthorId = authorId,
        ShowBlogPosts.sbpiStatus = status
      }

-- | Create tags for a show blog post
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
  ShowBlogPosts.Id ->
  NewShowBlogPostForm ->
  m ()
createPostTags postId form = do
  let tags = nsbpfTags form
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
  ShowBlogPosts.Id ->
  Text ->
  m ()
createOrAssociateTag postId tagName =
  execQuerySpan (ShowBlogTags.getShowBlogTagByName tagName) >>= \case
    Right (Just existingTag) -> do
      -- If tag exists, associate it
      void $ execQuerySpan (ShowBlogPosts.addTagToShowBlogPost postId (ShowBlogTags.sbtmId existingTag))
    _ -> do
      -- otherwise, create new tag and associate it
      tagInsertResult <- execQuerySpan (ShowBlogTags.insertShowBlogTag (ShowBlogTags.Insert tagName))
      case tagInsertResult of
        Right newTagId -> do
          void $ execQuerySpan (ShowBlogPosts.addTagToShowBlogPost postId newTagId)
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
  ShowBlogPosts.Insert ->
  NewShowBlogPostForm ->
  Slug ->
  m (Lucid.Html ())
handlePostCreation blogPostData form showSlug = do
  execQuerySpan (ShowBlogPosts.insertShowBlogPost blogPostData) >>= \case
    Left dbError -> do
      Log.logInfo ("Database error creating show blog post: " <> Text.pack (show dbError)) ()
      pure (errorTemplate showSlug "Database error occurred. Please try again.")
    Right postId -> do
      execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
        Right (Just createdPost) -> do
          createPostTags postId form
          Log.logInfo ("Successfully created show blog post: " <> ShowBlogPosts.title createdPost) ()
          pure (successTemplate showSlug createdPost)
        _ -> do
          Log.logInfo "Created blog post but failed to retrieve it" ()
          pure (errorTemplate showSlug "Post was created but there was an error displaying the confirmation.")
