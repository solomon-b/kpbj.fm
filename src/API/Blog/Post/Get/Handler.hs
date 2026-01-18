{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Post.Get.Handler where

--------------------------------------------------------------------------------

import API.Blog.Post.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Component.Redirect (redirectTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Either (fromRight)
import Data.Functor
import Data.Has (Has, getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for blog post with both ID and slug
handlerWithSlug ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env,
    Has StorageBackend env
  ) =>
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug tracer postId slug = handler tracer postId (Just slug)

-- | Handler for blog post with ID only (always redirects)
handlerWithoutSlug ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env,
    Has StorageBackend env
  ) =>
  Tracer ->
  BlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug tracer postId = handler tracer postId Nothing

-- | Shared handler for both routes
handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env,
    Has StorageBackend env
  ) =>
  Tracer ->
  BlogPosts.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer postId mUrlSlug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog post from database" postId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Blog post not found" postId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right (Just blogPost) -> do
      let canonicalSlug = BlogPosts.bpmSlug blogPost
          postIdText = display postId
          slugText = display canonicalSlug
          canonicalUrl = [i|/blog/#{postIdText}/#{slugText}|]

      if matchSlug canonicalSlug mUrlSlug
        then renderPost hxRequest mUserInfo blogPost canonicalSlug
        else renderRedirect hxRequest mUserInfo canonicalUrl

renderPost ::
  ( Has Tracer env,
    MonadReader env m,
    Log.MonadLog m,
    MonadDB m,
    MonadUnliftIO m,
    MonadCatch m,
    Has (Maybe GoogleAnalyticsId) env,
    Has StorageBackend env
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  BlogPosts.Model ->
  Slug ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderPost hxRequest mUserInfo blogPost canonicalSlug = do
  backend <- asks getter
  execQuerySpan (UserMetadata.getUserMetadata (BlogPosts.bpmAuthorId blogPost)) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog post author" (BlogPosts.bpmAuthorId blogPost)
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate canonicalSlug)
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Blog post author not found" (BlogPosts.bpmAuthorId blogPost)
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate canonicalSlug)
      pure $ Servant.noHeader html
    Right (Just author) -> do
      tagsResult <- execQuerySpan (BlogPosts.getTagsForPost (BlogPosts.bpmId blogPost))
      let tags = fromRight [] tagsResult
      renderedContent <- renderContentM (BlogPosts.bpmContent blogPost)
      let postTemplate = template backend blogPost author tags renderedContent
      html <- renderTemplate hxRequest mUserInfo postTemplate
      pure $ Servant.noHeader html

renderRedirect ::
  (MonadCatch m, Log.MonadLog m, Has (Maybe GoogleAnalyticsId) env, MonadReader env m) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo canonicalUrl = do
  Log.logInfo "Redirecting to canonical blog post URL" canonicalUrl
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate canonicalUrl)
  pure $ Servant.addHeader canonicalUrl html
