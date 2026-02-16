{-# LANGUAGE ViewPatterns #-}

module API.Shows.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Shows.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Shows.Get.Templates.Page (ShowsViewData (..), template)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

-- | Servant handler: thin glue composing action + render with request boilerplate.
handler ::
  Maybe PageNumber ->
  Maybe (Filter ShowTags.Id) ->
  Maybe (Filter Shows.Status) ->
  Maybe (Filter Search) ->
  Maybe (Filter ShowSortBy) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mPage mTag mStatus mSearch mSort (coerce -> cookie) (fromMaybe IsNotHxRequest -> hxRequest) =
  handleHtmlErrors "Shows list" apiLinks.rootGet $ do
    mUserInfo <- getUserInfo cookie <&> fmap snd
    let isAppendRequest = hxRequest == IsHxRequest && fromMaybe 1 mPage > 1
    viewData <- action mPage mTag mStatus mSearch mSort
    if isAppendRequest
      then pure (renderItemsFragment viewData)
      else renderTemplate hxRequest mUserInfo (template viewData)

--------------------------------------------------------------------------------

-- | Business logic: fetch tags, shows, compute pagination.
action ::
  Maybe PageNumber ->
  Maybe (Filter ShowTags.Id) ->
  Maybe (Filter Shows.Status) ->
  Maybe (Filter Search) ->
  Maybe (Filter ShowSortBy) ->
  AppM ShowsViewData
action (fromMaybe 1 -> page) maybeTagIdFilter maybeStatusFilter maybeSearchFilter maybeSortByFilter = do
  storageBackend <- asks getter
  let maybeTagId = maybeTagIdFilter >>= getFilter
      maybeStatus = maybeStatusFilter >>= getFilter
      maybeSearch = maybeSearchFilter >>= getFilter
      maybeSortBy = maybeSortByFilter >>= getFilter
      limit = 12 :: Limit
      offset = fromIntegral $ ((coerce page :: Int64) - 1) * fromIntegral limit :: Offset
      sortBy = fromMaybe NameAZ maybeSortBy

  allTags <-
    execQuery ShowTags.getShowTagsWithCounts >>= \case
      Left err -> throwDatabaseError err
      Right tags -> pure tags

  allShows <-
    getShows (limit + 1) offset maybeSearch maybeTagId maybeStatus sortBy >>= \case
      Left err -> throwDatabaseError err
      Right results -> pure results

  let someShows = take (fromIntegral limit) allShows
      hasMore = length allShows > fromIntegral limit

  pure
    ShowsViewData
      { svStorageBackend = storageBackend,
        svShows = someShows,
        svTags = allTags,
        svCurrentPage = page,
        svHasMore = hasMore,
        svTagFilter = maybeTagId,
        svStatusFilter = maybeStatus,
        svSearchFilter = maybeSearch,
        svSortByFilter = maybeSortBy
      }

getShows ::
  Limit ->
  Offset ->
  Maybe Search ->
  Maybe ShowTags.Id ->
  Maybe Shows.Status ->
  ShowSortBy ->
  AppM (Either HSQL.Pool.UsageError [Shows.Model])
getShows limit offset maybeSearch maybeTagId maybeStatus sortBy = do
  case maybeSearch of
    Just (Search searchTerm)
      | not (Text.null $ Text.strip searchTerm) ->
          -- If search term is provided, use search function (search has its own relevance-based sorting)
          execQuery (Shows.searchShows (Search $ Text.strip searchTerm) limit offset)
    _ ->
      -- No search term, use filter and sort logic
      execQuery (Shows.getShowsFiltered maybeTagId maybeStatus sortBy limit offset)
