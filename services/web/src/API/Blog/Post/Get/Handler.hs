{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Post.Get.Handler where

--------------------------------------------------------------------------------

import API.Blog.Post.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError (..), errorContent, errorRedirectParams, logHandlerError, notFoundContent, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Redirect (buildRedirectUrl, redirectTemplate, redirectWithBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

data BlogPostViewData
  = BlogPostRedirect Text
  | BlogPostContent StorageBackend BlogPosts.Model UserMetadata.Model [BlogTags.Model]

--------------------------------------------------------------------------------

-- | Handler for blog post with both ID and slug
handlerWithSlug ::
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug postId slug = handler postId (Just slug)

-- | Handler for blog post with ID only (always redirects)
handlerWithoutSlug ::
  BlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug postId = handler postId Nothing

-- | Shared handler for both routes
handler ::
  BlogPosts.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler postId mUrlSlug cookie (foldHxReq -> hxRequest) =
  runExceptT innerAction >>= \case
    Right result -> pure result
    Left err -> do
      logHandlerError "Blog post" err
      case err of
        NotFound resource -> Servant.noHeader <$> renderInline (notFoundContent resource)
        NotAuthenticated ->
          let (url, banner) = errorRedirectParams apiLinks.rootGet err
           in pure $ Servant.addHeader (buildRedirectUrl url banner) (redirectWithBanner url banner)
        NotAuthorized _ _ ->
          let (url, banner) = errorRedirectParams apiLinks.rootGet err
           in pure $ Servant.addHeader (buildRedirectUrl url banner) (redirectWithBanner url banner)
        DatabaseError _ -> Servant.noHeader <$> renderInline (errorContent "Something went wrong. Please try again.")
        UserSuspended -> Servant.noHeader <$> renderInline (errorContent "Your account is suspended.")
        ValidationError msg -> Servant.noHeader <$> renderInline (errorContent msg)
        HandlerFailure msg -> Servant.noHeader <$> renderInline (errorContent msg)
  where
    innerAction = do
      mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
      vd <- action postId mUrlSlug
      case vd of
        BlogPostRedirect canonicalUrl -> do
          html <- lift $ renderTemplate hxRequest mUserInfo (redirectTemplate canonicalUrl)
          pure $ Servant.addHeader canonicalUrl html
        BlogPostContent backend blogPost author tags -> do
          renderedContent <- lift $ renderContentM (BlogPosts.bpmContent blogPost)
          html <- lift $ renderTemplate hxRequest mUserInfo (template backend blogPost author tags renderedContent)
          pure $ Servant.noHeader html
    renderInline content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch blog post, verify slug, fetch author and tags.
action ::
  BlogPosts.Id ->
  Maybe Slug ->
  ExceptT HandlerError AppM BlogPostViewData
action postId mUrlSlug = do
  blogPost <-
    fromMaybeM (throwNotFound "Blog post") $
      fromRightM throwDatabaseError $
        execQuery (BlogPosts.getBlogPostById postId)

  let canonicalSlug = BlogPosts.bpmSlug blogPost
      postIdText = display postId
      slugText = display canonicalSlug
      canonicalUrl = [i|/blog/#{postIdText}/#{slugText}|]

  if matchSlug canonicalSlug mUrlSlug
    then do
      backend <- asks getter
      author <-
        fromMaybeM (throwNotFound "Blog post author") $
          fromRightM throwDatabaseError $
            execQuery (UserMetadata.getUserMetadata (BlogPosts.bpmAuthorId blogPost))
      tags <- fromRight [] <$> execQuery (BlogPosts.getTagsForPost (BlogPosts.bpmId blogPost))
      pure $ BlogPostContent backend blogPost author tags
    else pure $ BlogPostRedirect canonicalUrl
