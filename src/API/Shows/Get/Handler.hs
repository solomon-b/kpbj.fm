{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Shows.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Shows.Get.Templates.Page (template)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
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
  Maybe PageNumber ->
  Maybe (Filter ShowTags.Id) ->
  Maybe (Filter Shows.Status) ->
  Maybe (Filter Search) ->
  Maybe (Filter ShowSortBy) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer (fromMaybe 1 -> page) maybeTagIdFilter maybeStatusFilter maybeSearchFilter maybeSortByFilter (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  storageBackend <- asks getter
  let maybeTagId = maybeTagIdFilter >>= getFilter
      maybeStatus = maybeStatusFilter >>= getFilter
      maybeSearch = maybeSearchFilter >>= getFilter
      maybeSortBy = maybeSortByFilter >>= getFilter
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    let limit = 12 :: Limit
        offset = fromIntegral $ ((coerce page :: Int64) - 1) * fromIntegral limit :: Offset
        sortBy = fromMaybe NameAZ maybeSortBy
        -- Infinite scroll request = HTMX request for page > 1
        isAppendRequest = htmxRequest == IsHxRequest && page > 1

    -- Fetch all available tags for the filter UI
    execQuerySpan ShowTags.getShowTagsWithCounts >>= \case
      Left tagsErr -> do
        Log.logInfo "Failed to fetch tags from database" (Aeson.object ["error" .= show tagsErr])
        let banner = BannerParams Error "Error" "Failed to load shows. Please try again."
        renderTemplate htmxRequest mUserInfo (redirectWithBanner [i|/#{rootGetUrl}|] banner)
      Right allTags -> do
        -- Fetch limit + 1 to check if there are more results
        getShows (limit + 1) offset maybeSearch maybeTagId maybeStatus sortBy >>= \case
          Left err -> do
            Log.logInfo "Failed to fetch shows from database" (Aeson.object ["error" .= show err])
            let banner = BannerParams Error "Error" "Failed to load shows. Please try again."
            renderTemplate htmxRequest mUserInfo (redirectWithBanner [i|/#{rootGetUrl}|] banner)
          Right allShows -> do
            let someShows = take (fromIntegral limit) allShows
                hasMore = length allShows > fromIntegral limit

            if isAppendRequest
              then
                -- Infinite scroll: return only new items + sentinel (no page wrapper)
                pure $ renderItemsFragment storageBackend someShows page hasMore maybeTagId maybeStatus maybeSearch maybeSortBy
              else do
                -- Full page: render with header, items, sentinel, and noscript pagination
                let showsTemplate = template storageBackend someShows allTags page hasMore maybeTagId maybeStatus maybeSearch maybeSortBy
                renderTemplate htmxRequest mUserInfo showsTemplate

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
          execQuerySpan (Shows.searchShows (Search $ Text.strip searchTerm) limit offset)
    _ ->
      -- No search term, use filter and sort logic
      execQuerySpan (Shows.getShowsFiltered maybeTagId maybeStatus sortBy limit offset)
