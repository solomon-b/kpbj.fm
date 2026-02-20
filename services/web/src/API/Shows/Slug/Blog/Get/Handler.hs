{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module API.Shows.Slug.Blog.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handlePublicErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad (join)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

postsPerPage :: Limit
postsPerPage = 12

data ShowBlogViewData = ShowBlogViewData
  { sbvdStorageBackend :: StorageBackend,
    sbvdShowModel :: Shows.Model,
    sbvdPosts :: [ShowBlogPosts.Model],
    sbvdTags :: [ShowBlogTags.Model],
    sbvdMaybeTag :: Maybe Text,
    sbvdPage :: Int64,
    sbvdTotalPages :: Int64
  }

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Int64 ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler slug maybePage maybeTag cookie (foldHxReq -> hxRequest) =
  handlePublicErrors "Show blog" renderError $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action slug maybePage maybeTag
    lift $
      renderTemplate hxRequest mUserInfo $
        template
          vd.sbvdStorageBackend
          vd.sbvdShowModel
          vd.sbvdPosts
          vd.sbvdTags
          vd.sbvdMaybeTag
          vd.sbvdPage
          vd.sbvdTotalPages
  where
    renderError content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch show blog listing with optional tag filter.
action ::
  Slug ->
  Maybe Int64 ->
  Maybe Text ->
  ExceptT HandlerError AppM ShowBlogViewData
action slug maybePage maybeTag = do
  backend <- asks getter
  let page = fromMaybe 1 maybePage
      offset = fromIntegral $ (page - 1) * fromIntegral postsPerPage :: Offset

  showModel <-
    fromRightM throwDatabaseError (execQuery (Shows.getShowBySlug slug)) >>= \case
      Nothing -> throwNotFound (display slug)
      Just m -> pure m

  (posts, tags, totalPosts) <-
    fromRightM throwDatabaseError $
      lift $
        fetchData slug offset maybeTag

  let totalPages = (totalPosts + fromIntegral postsPerPage - 1) `div` fromIntegral postsPerPage

  pure
    ShowBlogViewData
      { sbvdStorageBackend = backend,
        sbvdShowModel = showModel,
        sbvdPosts = posts,
        sbvdTags = tags,
        sbvdMaybeTag = maybeTag,
        sbvdPage = page,
        sbvdTotalPages = totalPages
      }

fetchData :: Slug -> Offset -> Maybe Text -> AppM (Either HSQL.Pool.UsageError ([ShowBlogPosts.Model], [ShowBlogTags.Model], Int64))
fetchData slug offset maybeTag = runDBTransaction $ do
  TRX.statement () (Shows.getShowBySlug slug) >>= \case
    Just showModel -> do
      (posts, postCount) <- getPosts slug offset showModel maybeTag
      tags <- TRX.statement () $ ShowBlogPosts.getTagsForShow showModel.id
      pure (posts, tags, postCount)
    Nothing ->
      pure (mempty, mempty, 0)

getPosts :: Slug -> Offset -> Shows.Model -> Maybe Text -> TRX.Transaction ([ShowBlogPosts.Model], Int64)
getPosts slug offset showModel tagName =
  join <$> traverse (TRX.statement () . ShowBlogTags.getShowBlogTagByName) tagName >>= \case
    Just tag -> do
      publishedCount <- TRX.statement () $ ShowBlogPosts.countPublishedShowBlogPostsByTag showModel.id tag.sbtmId
      posts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPostsByShowAndTag showModel.id tag.sbtmId postsPerPage offset
      pure (posts, publishedCount)
    _ -> do
      publishedCount <- TRX.statement () $ ShowBlogPosts.countPublishedShowBlogPosts showModel.id
      posts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPostsBySlug (display slug) postsPerPage offset
      pure (posts, publishedCount)
