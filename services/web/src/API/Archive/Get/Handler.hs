{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Archive.Get.Handler where

--------------------------------------------------------------------------------

import API.Archive.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Archive.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

data ArchiveListViewData = ArchiveListViewData
  { alvdStorageBackend :: StorageBackend,
    alvdEpisodes :: [(Episodes.Model, Shows.Model)],
    alvdPage :: Int64,
    alvdHasMore :: Bool
  }

--------------------------------------------------------------------------------

handler ::
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Archive list" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    let page = fromMaybe 1 maybePage
        -- Infinite scroll request = HTMX request for page > 1
        isAppendRequest = hxRequest == IsHxRequest && page > 1
    vd <- action maybePage
    if isAppendRequest
      then pure $ renderItemsFragment vd.alvdStorageBackend vd.alvdEpisodes vd.alvdPage vd.alvdHasMore
      else lift $ renderTemplate hxRequest mUserInfo (template vd.alvdStorageBackend vd.alvdEpisodes vd.alvdPage vd.alvdHasMore)

--------------------------------------------------------------------------------

-- | Fetch a page of published episodes across all shows.
action ::
  Maybe Int64 ->
  ExceptT HandlerError AppM ArchiveListViewData
action maybePage = do
  storageBackend <- asks getter
  currentTime <- liftIO getCurrentTime
  let page = fromMaybe 1 maybePage
      limit = 12 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  allEpisodes <-
    fromRightM throwDatabaseError $
      lift (execQuery (Episodes.getPublishedEpisodesWithShows currentTime (limit + 1) offset))

  let episodes = take (fromIntegral limit) allEpisodes
      hasMore = length allEpisodes > fromIntegral limit

  pure
    ArchiveListViewData
      { alvdStorageBackend = storageBackend,
        alvdEpisodes = episodes,
        alvdPage = page,
        alvdHasMore = hasMore
      }
