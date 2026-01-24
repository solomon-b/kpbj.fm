{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Get.Handler where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Blog.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Int64 ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer maybePage maybeTag cookie (foldHxReq -> hxRequest) = do
  storageBackend <- asks getter
  let page = fromMaybe 1 maybePage
      limit = 10 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      -- Infinite scroll request = HTMX request for page > 1
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- Get user info once upfront
  mUserInfo <- getUserInfo cookie <&> fmap snd

  -- Get current time for relative date formatting
  currentTime <- liftIO getCurrentTime

  -- Get blog posts based on filters (tag only)
  getBlogPostResults limit offset maybeTag >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch blog posts from database" ()
      let banner = BannerParams Error "Error" "Failed to load blog posts. Please try again."
      renderTemplate hxRequest mUserInfo (redirectWithBanner [i|/#{rootGetUrl}|] banner)
    Right allPosts -> do
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit

      if isAppendRequest
        then
          -- Infinite scroll: return only new items + sentinel (no page wrapper)
          pure $ renderItemsFragment storageBackend currentTime posts page hasMore maybeTag
        else do
          -- Full page: render with header, items, sentinel, and noscript pagination
          let blogTemplate = template storageBackend currentTime posts page hasMore maybeTag
          renderTemplate hxRequest mUserInfo blogTemplate

getBlogPostResults ::
  Limit ->
  Offset ->
  Maybe Text ->
  AppM (Either HSQL.Pool.UsageError [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])])
getBlogPostResults limit offset maybeTag = do
  case maybeTag of
    Just tagName ->
      execQuerySpan (BlogTags.getTagByName tagName) >>= \case
        Left err ->
          pure (Left err)
        Right Nothing ->
          pure (Right [])
        Right (Just tag) ->
          getPostsWithTagsFiltered tag limit offset
    Nothing ->
      getPostsWithTags limit offset

getPostsWithTags ::
  Limit ->
  Offset ->
  AppM (Either HSQL.Pool.UsageError [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])])
getPostsWithTags limit offset =
  execTransactionSpan $ do
    posts <- HT.statement () $ BlogPosts.getPublishedBlogPosts limit offset
    forM posts $ \post -> do
      tags <- HT.statement () $ BlogPosts.getTagsForPost post.bpmId
      mAuthor <- HT.statement () $ UserMetadata.getUserMetadata post.bpmAuthorId
      let author = fromMaybe defaultAuthor mAuthor
      pure (post, author, tags)

getPostsWithTagsFiltered ::
  BlogTags.Model ->
  Limit ->
  Offset ->
  AppM (Either HSQL.Pool.UsageError [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])])
getPostsWithTagsFiltered tag limit offset =
  execTransactionSpan $ do
    posts <- HT.statement () $ BlogPosts.getPostsByTag tag.btmId limit offset
    forM posts $ \post -> do
      tags <- HT.statement () $ BlogPosts.getTagsForPost post.bpmId
      mAuthor <- HT.statement () $ UserMetadata.getUserMetadata post.bpmAuthorId
      let author = fromMaybe defaultAuthor mAuthor
      pure (post, author, tags)

-- | Default author for posts with missing author metadata
defaultAuthor :: UserMetadata.Model
defaultAuthor =
  UserMetadata.Model
    { UserMetadata.mId = UserMetadata.Id 0,
      UserMetadata.mUserId = 0,
      UserMetadata.mDisplayName = mkDisplayNameUnsafe "Unknown Author",
      UserMetadata.mFullName = mkFullNameUnsafe "Unknown",
      UserMetadata.mAvatarUrl = Nothing,
      UserMetadata.mUserRole = UserMetadata.User,
      UserMetadata.mColorScheme = UserMetadata.Automatic,
      UserMetadata.mTheme = UserMetadata.Default,
      UserMetadata.mSuspensionStatus = UserMetadata.NotSuspended
    }
