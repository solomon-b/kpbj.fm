{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module API.Shows.Slug.Blog.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Control.Monad (join)
import Control.Monad.Reader (asks)
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
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

postsPerPage :: Limit
postsPerPage = 12

handler ::
  Tracer ->
  Slug ->
  Maybe Int64 ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer slug maybePage maybeTag cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      offset = fromIntegral $ (page - 1) * fromIntegral postsPerPage :: Offset

  storageBackend <- asks getter
  mUserInfo <- fmap snd <$> getUserInfo cookie

  execQuerySpan (Shows.getShowBySlug slug) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Show not found: " <> display slug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just showModel) -> do
      -- Fetch blog posts (with optional tag filter)
      fetchData slug offset maybeTag >>= \case
        Right (posts, tags, totalPosts) -> do
          let totalPages = (totalPosts + fromIntegral postsPerPage - 1) `div` fromIntegral postsPerPage
              pageTemplate = template storageBackend showModel posts tags maybeTag page totalPages
          renderTemplate hxRequest mUserInfo pageTemplate
        _ -> do
          -- If queries fail, show with empty data
          let pageTemplate = template storageBackend showModel [] [] maybeTag page 0
          renderTemplate hxRequest mUserInfo pageTemplate

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
