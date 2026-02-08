module API.Dashboard.Blogs.New.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Post.Route (NewShowBlogPostForm (..))
import API.Links (dashboardBlogsLinks, rootLink)
import API.Types (DashboardBlogsRoutes (..))
import App.Handler.Combinators (requireAuth, requireHostNotSuspended, requireRight)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotAuthorized)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (guard, unless, void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Cookie ->
  NewShowBlogPostForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showSlug cookie form =
  handleRedirectErrors "Show blog post creation" (dashboardBlogsLinks.newGet showSlug) $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to create blog posts." userMetadata

    -- 2. Fetch show and verify host permissions in a transaction
    showModel <- fetchShowBySlug user userMetadata showSlug

    -- 3. Validate and create blog post
    blogPostData <-
      requireRight
        Sanitize.displayContentValidationError
        (validateNewShowBlogPost form ((.id) showModel) (UserMetadata.mUserId userMetadata))
    handlePostCreation blogPostData form showModel

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

fetchShowBySlug ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  AppM Shows.Model
fetchShowBySlug user userMetadata showSlug = do
  mResult <- execTransaction $ runMaybeT $ do
    showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
    -- Admins can create blog posts for any show, hosts need explicit assignment
    unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
      isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
      guard isHost
    pure showModel

  case mResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotAuthorized "You must be a host of this show to create blog posts." (Just userMetadata.mUserRole)
    Right (Just sm) -> pure sm

-- | Create tags for a show blog post
createPostTags ::
  ShowBlogPosts.Id ->
  NewShowBlogPostForm ->
  AppM ()
createPostTags postId form = do
  let tags = nsbpfTags form
  unless (null tags) $
    traverse_ (createOrAssociateTag postId) tags

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  ShowBlogPosts.Id ->
  Text ->
  AppM ()
createOrAssociateTag postId tagName =
  execQuery (ShowBlogTags.getShowBlogTagByName tagName) >>= \case
    Right (Just existingTag) -> do
      -- If tag exists, associate it
      void $ execQuery (ShowBlogPosts.addTagToShowBlogPost postId (ShowBlogTags.sbtmId existingTag))
    _ -> do
      -- otherwise, create new tag and associate it
      tagInsertResult <- execQuery (ShowBlogTags.insertShowBlogTag (ShowBlogTags.Insert tagName))
      case tagInsertResult of
        Right (Just newTagId) -> do
          void $ execQuery (ShowBlogPosts.addTagToShowBlogPost postId newTagId)
        Right Nothing -> do
          Log.logInfo ("Tag insert returned Nothing: " <> tagName) ()
        Left dbError -> do
          Log.logInfo ("Database error creating tag: " <> Text.pack (show dbError)) ()
          pure ()

-- | Handle blog post creation after validation passes
handlePostCreation ::
  ShowBlogPosts.Insert ->
  NewShowBlogPostForm ->
  Shows.Model ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlePostCreation blogPostData form showModel = do
  postId <-
    execQuery (ShowBlogPosts.insertShowBlogPost blogPostData) >>= \case
      Left err -> throwDatabaseError err
      Right (Just pid) -> pure pid
      Right Nothing -> throwHandlerFailure "Show blog post insert returned Nothing"

  createPostTags postId form
  Log.logInfo "Successfully created show blog post" postId

  let detailUrl = rootLink $ dashboardBlogsLinks.detail (Shows.slug showModel) postId
      banner = BannerParams Success "Blog Post Created" "Your blog post has been created successfully."
      redirectUrl = buildRedirectUrl detailUrl banner
  pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
