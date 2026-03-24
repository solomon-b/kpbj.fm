{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Post.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Shows.Slug.Blog.Post.Get.Templates.Page (template)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError (..), errorContent, errorRedirectParams, logHandlerError, notFoundContent, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Flash (throwHxRedirect)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

data ShowBlogPostViewData = ShowBlogPostViewData
  { sbpvdShowModel :: Shows.Model,
    sbpvdPost :: ShowBlogPosts.Model,
    sbpvdAuthor :: UserMetadata.Model,
    sbpvdTags :: [ShowBlogTags.Model]
  }

-- | Outcome of slug matching: render the post or redirect to canonical URL.
data ShowBlogPostOutcome
  = RenderPost ShowBlogPostViewData
  | RedirectTo Text

--------------------------------------------------------------------------------

-- | Handler for show blog post with show ID, post ID, and slug.
handlerWithSlug ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handlerWithSlug showId postId slug = handler showId postId (Just slug)

-- | Handler for show blog post with show ID and post ID only (always redirects).
handlerWithoutSlug ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handlerWithoutSlug showId postId = handler showId postId Nothing

-- | Shared handler for both routes.
handler ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showId postId mUrlSlug cookie (foldHxReq -> hxRequest) =
  runExceptT innerAction >>= \case
    Right result -> pure result
    Left err -> do
      logHandlerError "Show blog post" err
      case err of
        NotFound resource -> renderInline (notFoundContent resource)
        NotAuthenticated ->
          let (url, flash) = errorRedirectParams apiLinks.rootGet err
           in throwHxRedirect url (Just flash)
        NotAuthorized _ _ ->
          let (url, flash) = errorRedirectParams apiLinks.rootGet err
           in throwHxRedirect url (Just flash)
        DatabaseError _ -> renderInline (errorContent "Something went wrong. Please try again.")
        UserSuspended -> renderInline (errorContent "Your account is suspended.")
        ValidationError msg -> renderInline (errorContent msg)
        HandlerFailure msg -> renderInline (errorContent msg)
  where
    innerAction = do
      mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
      outcome <- action showId postId mUrlSlug
      case outcome of
        RenderPost vd -> do
          renderedContent <- lift $ renderContentM (ShowBlogPosts.content vd.sbpvdPost)
          lift $ renderTemplate hxRequest mUserInfo $ template vd.sbpvdShowModel vd.sbpvdPost vd.sbpvdAuthor vd.sbpvdTags renderedContent
        RedirectTo url ->
          lift $ throwHxRedirect url Nothing
    renderInline content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch show blog post and determine render vs redirect.
action ::
  Shows.Id ->
  ShowBlogPosts.Id ->
  Maybe Slug ->
  ExceptT HandlerError AppM ShowBlogPostOutcome
action showId postId mUrlSlug = do
  post <-
    fromMaybeM (throwNotFound "Blog post") $
      fromRightM throwDatabaseError $
        execQuery (ShowBlogPosts.getShowBlogPostById postId)

  if post.showId /= showId
    then throwNotFound "Blog post"
    else do
      let canonicalSlug = ShowBlogPosts.slug post
          showIdText = display showId
          postIdText = display postId
          slugText = display canonicalSlug
          canonicalUrl = [i|/shows/#{showIdText}/blog/#{postIdText}/#{slugText}|]

      if matchSlug canonicalSlug mUrlSlug
        then RenderPost <$> fetchPostViewData post
        else pure $ RedirectTo canonicalUrl

-- | Fetch all data needed to render a show blog post detail page.
fetchPostViewData ::
  ShowBlogPosts.Model ->
  ExceptT HandlerError AppM ShowBlogPostViewData
fetchPostViewData post = do
  showModel <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowById post.showId)

  author <-
    fromMaybeM (throwNotFound "Author") $
      fromRightM throwDatabaseError $
        execQuery (UserMetadata.getUserMetadata post.authorId)

  tagsResult <- execQuery (ShowBlogPosts.getTagsForShowBlogPost post.id)
  let tags = fromRight [] tagsResult

  pure
    ShowBlogPostViewData
      { sbpvdShowModel = showModel,
        sbpvdPost = post,
        sbpvdAuthor = author,
        sbpvdTags = tags
      }
