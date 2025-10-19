{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Post.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogGetLink, showBlogPostGetLink, showGetLink)
import API.Shows.Slug.Blog.Post.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
showBlogPostGetUrl :: Slug -> Slug -> Links.URI
showBlogPostGetUrl showSlug postSlug = Links.linkURI $ showBlogPostGetLink showSlug postSlug

showBlogGetUrl :: Slug -> Links.URI
showBlogGetUrl slug = Links.linkURI $ showBlogGetLink slug Nothing Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug/blog/:postSlug"
    ( "shows"
        :> Servant.Capture "slug" Slug
        :> "blog"
        :> Servant.Capture "postSlug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug postSlug cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult

  -- Fetch the blog post (which includes show join)
  postResult <- execQuerySpan (ShowBlogPosts.getShowBlogPostBySlug (display showSlug) (display postSlug))
  case postResult of
    Left err -> do
      Log.logInfo "Failed to fetch blog post from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load blog post. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Blog post not found: " <> display showSlug <> "/" <> display postSlug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug postSlug)
    Right (Just post) -> do
      -- Fetch the show
      showResult <- execQuerySpan (Shows.getShowById post.showId)

      -- Fetch author info
      authorResult <- execQuerySpan (UserMetadata.getUserMetadata post.authorId)

      -- Fetch tags for this post
      tagsResult <- execQuerySpan (ShowBlogPosts.getTagsForShowBlogPost post.id)

      case (showResult, authorResult) of
        (Right (Just showModel), Right (Just author)) -> do
          let tags = fromRight [] tagsResult
          let postTemplate = template showModel post author tags
          renderTemplate hxRequest mUserInfo postTemplate
        (Right Nothing, _) -> do
          Log.logInfo ("Show not found for post: " <> display showSlug) ()
          renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug postSlug)
        (_, Right Nothing) -> do
          Log.logInfo ("Author not found for post: " <> display postSlug) ()
          renderTemplate hxRequest mUserInfo (errorTemplate "Author information unavailable.")
        _ -> do
          Log.logInfo "Failed to fetch show or author" ()
          renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load blog post. Please try again.")
