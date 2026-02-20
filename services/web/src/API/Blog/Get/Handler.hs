{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Get.Handler where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Blog.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

data BlogListViewData = BlogListViewData
  { blvdStorageBackend :: StorageBackend,
    blvdPosts :: [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])],
    blvdCurrentTime :: UTCTime,
    blvdPage :: Int64,
    blvdHasMore :: Bool,
    blvdMaybeTag :: Maybe Text
  }

--------------------------------------------------------------------------------

handler ::
  Maybe Int64 ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage maybeTag cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Blog list" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    let page = fromMaybe 1 maybePage
        -- Infinite scroll request = HTMX request for page > 1
        isAppendRequest = hxRequest == IsHxRequest && page > 1
    vd <- action maybePage maybeTag
    if isAppendRequest
      then pure $ renderItemsFragment vd.blvdStorageBackend vd.blvdCurrentTime vd.blvdPosts vd.blvdPage vd.blvdHasMore vd.blvdMaybeTag
      else lift $ renderTemplate hxRequest mUserInfo (template vd.blvdStorageBackend vd.blvdCurrentTime vd.blvdPosts vd.blvdPage vd.blvdHasMore vd.blvdMaybeTag)

--------------------------------------------------------------------------------

-- | Business logic: fetch blog posts with optional tag filter and pagination.
action ::
  Maybe Int64 ->
  Maybe Text ->
  ExceptT HandlerError AppM BlogListViewData
action maybePage maybeTag = do
  storageBackend <- asks getter
  currentTime <- liftIO getCurrentTime
  let page = fromMaybe 1 maybePage
      limit = 10 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  allPosts <-
    fromRightM throwDatabaseError $
      lift (getBlogPostResults (limit + 1) offset maybeTag)

  let posts = take (fromIntegral limit) allPosts
      hasMore = length allPosts > fromIntegral limit

  pure
    BlogListViewData
      { blvdStorageBackend = storageBackend,
        blvdPosts = posts,
        blvdCurrentTime = currentTime,
        blvdPage = page,
        blvdHasMore = hasMore,
        blvdMaybeTag = maybeTag
      }

--------------------------------------------------------------------------------

getBlogPostResults ::
  Limit ->
  Offset ->
  Maybe Text ->
  AppM (Either HSQL.Pool.UsageError [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])])
getBlogPostResults limit offset maybeTag = do
  case maybeTag of
    Just tagName ->
      execQuery (BlogTags.getTagByName tagName) >>= \case
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
  execTransaction $ do
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
  execTransaction $ do
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
      UserMetadata.mTheme = UserMetadata.DefaultTheme,
      UserMetadata.mSuspensionStatus = UserMetadata.NotSuspended
    }
