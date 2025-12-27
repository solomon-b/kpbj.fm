{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.New.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Post.Route (NewShowBlogPostForm (..))
import API.Links (dashboardBlogsLinks, userLinks)
import API.Types (DashboardBlogsRoutes (..), UserRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (guard, unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.PostStatus (BlogPostStatus (..), decodeBlogPost)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardBlogsGetUrl :: Slug -> Text
dashboardBlogsGetUrl showSlug =
  let uri = Links.linkURI $ dashboardBlogsLinks.list showSlug Nothing
   in [i|/#{uri}|]

dashboardBlogsNewGetUrl :: Slug -> Text
dashboardBlogsNewGetUrl showSlug =
  let uri = Links.linkURI $ dashboardBlogsLinks.newGet showSlug
   in [i|/#{uri}|]

dashboardBlogsDetailUrl :: Slug -> ShowBlogPosts.Id -> Text
dashboardBlogsDetailUrl showSlug postId =
  let uri = Links.linkURI $ dashboardBlogsLinks.detail showSlug postId
   in [i|/#{uri}|]

userLoginGetUrl :: Text
userLoginGetUrl =
  let uri = Links.linkURI $ userLinks.loginGet Nothing Nothing
   in [i|/#{uri}|]

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
  NewShowBlogPostForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug cookie form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to create blog posts."
      pure $ Servant.noHeader (redirectWithBanner userLoginGetUrl banner)
    Just (_user, userMetadata)
      | UserMetadata.isSuspended userMetadata -> do
          let banner = BannerParams Error "Account Suspended" "You have been suspended."
          pure $ Servant.noHeader (redirectWithBanner (dashboardBlogsGetUrl showSlug) banner)
    Just (user, userMetadata) -> do
      -- Fetch show and verify host permissions in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
        -- Admins can create blog posts for any show, hosts need explicit assignment
        unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
          isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
          guard isHost
        pure showModel

      case mResult of
        Left err -> do
          Log.logAttention "Failed to process show blog post creation" (show err)
          let banner = BannerParams Error "Error" "Failed to create blog post. Please try again."
          pure $ Servant.noHeader (redirectWithBanner (dashboardBlogsNewGetUrl showSlug) banner)
        Right Nothing -> do
          Log.logInfo "Show not found or user not authorized" (showSlug, User.mId user)
          let banner = BannerParams Error "Permission Denied" "You must be a host of this show to create blog posts."
          pure $ Servant.noHeader (redirectWithBanner (dashboardBlogsGetUrl showSlug) banner)
        Right (Just showModel) -> do
          case validateNewShowBlogPost form showModel.id (UserMetadata.mUserId userMetadata) of
            Left validationError -> do
              let errorMsg = Sanitize.displayContentValidationError validationError
              Log.logInfo ("Show blog post creation failed: " <> errorMsg) ()
              let banner = BannerParams Error "Validation Error" errorMsg
              pure $ Servant.noHeader (redirectWithBanner (dashboardBlogsNewGetUrl showSlug) banner)
            Right blogPostData ->
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
  Shows.Model ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlePostCreation blogPostData form showModel = do
  execQuerySpan (ShowBlogPosts.insertShowBlogPost blogPostData) >>= \case
    Left dbError -> do
      Log.logInfo ("Database error creating show blog post: " <> Text.pack (show dbError)) ()
      let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
      pure $ Servant.noHeader (redirectWithBanner (dashboardBlogsNewGetUrl (Shows.slug showModel)) banner)
    Right postId -> do
      createPostTags postId form
      Log.logInfo "Successfully created show blog post" postId
      let detailUrl = dashboardBlogsDetailUrl (Shows.slug showModel) postId
          banner = BannerParams Success "Blog Post Created" "Your blog post has been created successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
