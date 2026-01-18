{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Post.Get.Handler where

--------------------------------------------------------------------------------

import API.Blog.Post.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Functor
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for blog post with both ID and slug
handlerWithSlug ::
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug tracer postId slug = handler tracer postId (Just slug)

-- | Handler for blog post with ID only (always redirects)
handlerWithoutSlug ::
  Tracer ->
  BlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug tracer postId = handler tracer postId Nothing

-- | Shared handler for both routes
handler ::
  Tracer ->
  BlogPosts.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
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
  HxRequest ->
  Maybe UserMetadata.Model ->
  BlogPosts.Model ->
  Slug ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
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
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo canonicalUrl = do
  Log.logInfo "Redirecting to canonical blog post URL" canonicalUrl
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate canonicalUrl)
  pure $ Servant.addHeader canonicalUrl html
