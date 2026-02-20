{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handlePublicErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Lucid qualified
import Rel8 (Result)
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Number of episodes to show per page
episodesPerPage :: Int64
episodesPerPage = 10

data ShowViewData = ShowViewData
  { svdStorageBackend :: StorageBackend,
    svdShowModel :: Shows.Model,
    svdEpisodes :: [Episodes.Model],
    svdHosts :: [ShowHost.ShowHostWithUser],
    svdSchedule :: [ShowSchedule.ScheduleTemplate Result],
    svdBlogPosts :: [ShowBlogPosts.Model],
    svdTags :: [ShowTags.Model],
    svdPage :: Int
  }

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Int ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler slug mPage cookie (foldHxReq -> hxRequest) =
  handlePublicErrors "Show detail" renderError $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action slug mPage
    lift $
      renderTemplate hxRequest mUserInfo $
        template
          vd.svdStorageBackend
          vd.svdShowModel
          vd.svdEpisodes
          vd.svdHosts
          vd.svdSchedule
          vd.svdBlogPosts
          vd.svdTags
          vd.svdPage
  where
    renderError content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch show and all related details.
action ::
  Slug ->
  Maybe Int ->
  ExceptT HandlerError AppM ShowViewData
action slug mPage = do
  backend <- asks getter
  let page = fromMaybe 1 mPage
      limit = fromIntegral episodesPerPage
      offset = fromIntegral (page - 1) * fromIntegral episodesPerPage

  sm <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowBySlug slug)

  now <- lift currentSystemTime
  (hosts, schedule, episodes, blogPosts, tags) <-
    fromRightM throwDatabaseError $
      lift $
        fetchShowDetails now sm limit offset
  pure
    ShowViewData
      { svdStorageBackend = backend,
        svdShowModel = sm,
        svdEpisodes = episodes,
        svdHosts = hosts,
        svdSchedule = schedule,
        svdBlogPosts = blogPosts,
        svdTags = tags,
        svdPage = page
      }

fetchShowDetails ::
  UTCTime ->
  Shows.Model ->
  Limit ->
  Offset ->
  AppM (Either HSQL.Pool.UsageError ([ShowHost.ShowHostWithUser], [ShowSchedule.ScheduleTemplate Result], [Episodes.Model], [ShowBlogPosts.Model], [ShowTags.Model]))
fetchShowDetails now showModel limit offset = runDBTransaction $ do
  episodes <- TRX.statement () $ Episodes.getPublishedEpisodesForShow now showModel.id limit offset
  hosts <- TRX.statement () $ ShowHost.getShowHostsWithUsers showModel.id
  blogPosts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0
  schedule <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showModel.id
  tags <- TRX.statement () $ Shows.getTagsForShow showModel.id
  pure (hosts, schedule, episodes, blogPosts, tags)
