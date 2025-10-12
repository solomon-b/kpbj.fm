{-# LANGUAGE ViewPatterns #-}

module API.Blog.Post.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink)
import API.Blog.Post.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Functor
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
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
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing (Just tag)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /blog/:slug"
    ( "blog"
        :> Servant.Capture "slug" Text
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
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuerySpan (BlogPosts.getBlogPostBySlug slug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog post from database" slug
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Blog post not found" slug
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just blogPost) -> do
      execQuerySpan (UserMetadata.getUserMetadata (BlogPosts.bpmAuthorId blogPost)) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch blog post author" (BlogPosts.bpmAuthorId blogPost)
          renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
        Right Nothing -> do
          Log.logInfo "Blog post author not found" (BlogPosts.bpmAuthorId blogPost)
          renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
        Right (Just author) -> do
          tagsResult <- execQuerySpan (BlogPosts.getTagsForPost (BlogPosts.bpmId blogPost))
          let tags = fromRight [] tagsResult
              postTemplate = template blogPost author tags
          renderTemplate hxRequest mUserInfo postTemplate
