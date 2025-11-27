{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug/blog"
    ( "shows"
        :> Servant.Capture "slug" Slug
        :> "blog"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "tag" Text
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

postsPerPage :: Limit
postsPerPage = 12

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
  Maybe Int64 ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug maybePage maybeTag cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult
  let page = fromMaybe 1 maybePage
  let offset = (page - 1) * fromIntegral postsPerPage

  showResult <- execQuerySpan (Shows.getShowBySlug slug)
  case showResult of
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Show not found: " <> display slug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just showModel) -> do
      -- Check if user is a host of this show
      isHost <- case userInfoResult of
        Nothing -> pure False
        Just (user, _userMetadata) ->
          fromRight False <$> execQuerySpan (ShowHost.isUserHostOfShow (User.mId user) showModel.id)

      -- Fetch blog posts (with optional tag filter)
      blogPostsResult <- case maybeTag of
        Nothing ->
          execQuerySpan (ShowBlogPosts.getPublishedShowBlogPostsBySlug (display slug) postsPerPage offset)
        Just tagName -> do
          -- First get the tag ID
          tagResult <- execQuerySpan (ShowBlogTags.getShowBlogTagByName tagName)
          case tagResult of
            Right (Just tag) ->
              execQuerySpan (ShowBlogPosts.getPublishedShowBlogPostsByShowAndTag showModel.id tag.sbtmId postsPerPage offset)
            _ -> pure $ Right [] -- If tag not found, return empty list

      -- Fetch all tags for this show
      tagsResult <- execQuerySpan (ShowBlogPosts.getTagsForShow showModel.id)

      -- Count total posts for pagination
      totalPostsResult <- case maybeTag of
        Nothing ->
          execQuerySpan (ShowBlogPosts.countPublishedShowBlogPosts showModel.id)
        Just tagName -> do
          tagResult <- execQuerySpan (ShowBlogTags.getShowBlogTagByName tagName)
          case tagResult of
            Right (Just tag) ->
              execQuerySpan (ShowBlogPosts.countPublishedShowBlogPostsByTag showModel.id tag.sbtmId)
            _ -> pure $ Right 0

      case (blogPostsResult, tagsResult, totalPostsResult) of
        (Right posts, Right tags, Right totalPosts) -> do
          let totalPages = (totalPosts + fromIntegral postsPerPage - 1) `div` fromIntegral postsPerPage
          let pageTemplate = template showModel posts tags maybeTag page totalPages isHost
          renderTemplate hxRequest mUserInfo pageTemplate
        _ -> do
          -- If queries fail, show with empty data
          let posts = fromRight [] blogPostsResult
          let tags = fromRight [] tagsResult
          let totalPosts = fromRight 0 totalPostsResult
          let totalPages = (totalPosts + fromIntegral postsPerPage - 1) `div` fromIntegral postsPerPage
          let pageTemplate = template showModel posts tags maybeTag page totalPages isHost
          renderTemplate hxRequest mUserInfo pageTemplate
