{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Post.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Post.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for show blog post with show ID, post ID, and slug
handlerWithSlug ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug showId postId slug = handler showId postId (Just slug)

-- | Handler for show blog post with show ID and post ID only (always redirects)
handlerWithoutSlug ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug showId postId = handler showId postId Nothing

-- | Shared handler for both routes
handler ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showId postId mUrlSlug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuery (ShowBlogPosts.getShowBlogPostById postId) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch show blog post from database" postId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown") (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Show blog post not found" postId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown") (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right (Just post) -> do
      -- Verify the show ID matches
      if post.showId /= showId
        then do
          Log.logInfo "Show ID mismatch for blog post" (postId, showId, post.showId)
          html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown") (mkSlug "unknown"))
          pure $ Servant.noHeader html
        else do
          let canonicalSlug = ShowBlogPosts.slug post
              showIdText = display showId
              postIdText = display postId
              slugText = display canonicalSlug
              canonicalUrl = [i|/shows/#{showIdText}/blog/#{postIdText}/#{slugText}|]

          if matchSlug canonicalSlug mUrlSlug
            then renderPost hxRequest mUserInfo post
            else renderRedirect hxRequest mUserInfo canonicalUrl

renderPost ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  ShowBlogPosts.Model ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderPost hxRequest mUserInfo post = do
  showResult <- execQuery (Shows.getShowById post.showId)
  authorResult <- execQuery (UserMetadata.getUserMetadata post.authorId)
  tagsResult <- execQuery (ShowBlogPosts.getTagsForShowBlogPost post.id)

  case (showResult, authorResult) of
    (Right (Just showModel), Right (Just author)) -> do
      let tags = fromRight [] tagsResult
      renderedContent <- renderContentM (ShowBlogPosts.content post)
      let postTemplate = template showModel post author tags renderedContent
      html <- renderTemplate hxRequest mUserInfo postTemplate
      pure $ Servant.noHeader html
    (Right Nothing, _) -> do
      Log.logInfo "Show not found for blog post" post.showId
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Show not found.")
      pure $ Servant.noHeader html
    (_, Right Nothing) -> do
      Log.logInfo "Author not found for blog post" post.authorId
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Author information unavailable.")
      pure $ Servant.noHeader html
    _ -> do
      Log.logInfo "Failed to fetch show or author for blog post" post.id
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load blog post. Please try again.")
      pure $ Servant.noHeader html

renderRedirect ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo url = do
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate url)
  pure $ Servant.addHeader url html
