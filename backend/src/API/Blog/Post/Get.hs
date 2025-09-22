{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Post.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink)
import App.Auth qualified as Auth
import Component.Frame (UserInfo (..), loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Blog qualified as Blog
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing Nothing (Just tag)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /blog/:slug"
    ( "blog"
        :> Servant.Capture "slug" Text
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render blog post content with simple paragraph breaks
renderBlogContent :: Text -> Lucid.Html ()
renderBlogContent content = do
  let paragraphs = Text.splitOn "\n\n" content
  Lucid.div_ [Lucid.class_ "prose max-w-none text-gray-700 leading-relaxed space-y-6"] $ do
    mapM_ renderParagraph paragraphs
  where
    renderParagraph para =
      if Text.null (Text.strip para)
        then pure ()
        else Lucid.p_ $ Lucid.toHtml para

-- | Render tags for a blog post
renderTags :: [Blog.BlogTagDomain] -> Lucid.Html ()
renderTags tags = do
  Lucid.div_ [Lucid.class_ "flex gap-2 mb-6"] $ do
    mapM_ renderTag tags
  where
    renderTag :: Blog.BlogTagDomain -> Lucid.Html ()
    renderTag tag =
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetTagUrl (Blog.btdName tag)}|],
          hxGet_ [i|/#{blogGetTagUrl (Blog.btdName tag)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-mono hover:bg-gray-300 cursor-pointer"
        ]
        $ Lucid.toHtml
        $ "#" <> Blog.btdName tag

-- | Main blog post template
template :: Blog.BlogPostModel -> UserMetadata.Domain -> [Blog.BlogTagDomain] -> Lucid.Html ()
template post author tags = do
  -- Blog Post Content
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 w-full"] $ do
    -- Post Header
    Lucid.header_ [Lucid.class_ "mb-8"] $ do
      -- Category badge
      Lucid.div_ [Lucid.class_ "mb-4"] $ do
        Lucid.span_ [Lucid.class_ "bg-green-200 text-green-800 px-3 py-1 text-sm font-bold"] $
          Lucid.toHtml (Blog.bpmCategory post)

      -- Title
      Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4 leading-tight"] $
        Lucid.toHtml (Blog.bpmTitle post)

      -- Metadata
      Lucid.div_ [Lucid.class_ "flex items-center gap-6 text-sm text-gray-600 mb-6"] $ do
        Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
          Lucid.div_ [Lucid.class_ "w-8 h-8 bg-gray-300 rounded-full flex items-center justify-center text-xs"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.dDisplayName author))
          Lucid.span_ $ do
            "By "
            Lucid.span_ [Lucid.class_ "font-bold text-gray-800"] $
              Lucid.toHtml (display (UserMetadata.dDisplayName author))

        case Blog.bpmPublishedAt post of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            Lucid.span_ $ Lucid.toHtml dateStr
          Nothing -> Lucid.span_ "Draft"

      -- Tags
      when (not (null tags)) $
        renderTags tags

    -- Post Content
    renderBlogContent (Blog.bpmContent post)

    -- Post Footer
    Lucid.footer_ [Lucid.class_ "mt-8 pt-8 border-t border-gray-300"] $ do
      -- Author Bio
      Lucid.div_ [Lucid.class_ "bg-gray-50 p-6 border-l-4 border-gray-800"] $ do
        Lucid.div_ [Lucid.class_ "flex items-start gap-4"] $ do
          Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 rounded-full flex items-center justify-center text-xl"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.dDisplayName author))
          Lucid.div_ $ do
            Lucid.h3_ [Lucid.class_ "font-bold text-lg mb-2"] $
              Lucid.toHtml (display (UserMetadata.dDisplayName author))
            Lucid.p_ [Lucid.class_ "text-sm text-gray-600 leading-relaxed"] $
              Lucid.toHtml $
                Text.pack $
                  "KPBJ " <> show (UserMetadata.dUserRole author) <> " • " <> Text.unpack (display (UserMetadata.dFullName author))

  -- Navigation
  Lucid.div_ [Lucid.class_ "mt-8 text-center"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO BLOG"
  where
    when True action = action
    when False _ = pure ()

-- | Template for when blog post is not found
notFoundTemplate :: Text -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "Blog Post Not Found"
    Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] $ do
      "The blog post with slug \""
      Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ Lucid.toHtml slug
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO BLOG"

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserInfo -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

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
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer slug cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  -- Get user info once upfront
  loginState <- Auth.userLoginState cookie
  mUserInfo <- case loginState of
    Auth.IsNotLoggedIn -> pure Nothing
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right (Just userMetadata) ->
          pure $ Just $ UserInfo {userDisplayName = UserMetadata.mDisplayName userMetadata}
        _ -> pure Nothing

  execQuerySpan (Blog.getBlogPostBySlug slug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog post from database" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Blog post not found" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just blogPost) -> do
      execQuerySpan (UserMetadata.getUserMetadata (Blog.bpmAuthorId blogPost)) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch blog post author" (Blog.bpmAuthorId blogPost)
          renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
        Right Nothing -> do
          Log.logInfo "Blog post author not found" (Blog.bpmAuthorId blogPost)
          renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
        Right (Just author) -> do
          tagsResult <- execQuerySpan (Blog.getTagsForPost (Blog.bpmId blogPost))
          let tags = case tagsResult of
                Left _err -> []
                Right tagModels -> map Blog.toDomainBlogTag tagModels
              postTemplate = template blogPost (UserMetadata.toDomain author) tags
          renderTemplate isHtmxRequest mUserInfo postTemplate
