{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new show cards and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Shows.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Types
import Component.Card.Show (renderShowCard)
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.String.Interpolate (i)
import Domain.Types.Filter (Filter (..))
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Domain.Types.ShowSortBy (ShowSortBy)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the show items and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing items container.
renderItemsFragment ::
  StorageBackend ->
  [Shows.Model] ->
  PageNumber ->
  Bool ->
  Maybe ShowTags.Id ->
  Maybe Shows.Status ->
  Maybe Search ->
  Maybe ShowSortBy ->
  Lucid.Html ()
renderItemsFragment backend allShows currentPage hasMore maybeTagId maybeStatus maybeSearch maybeSortBy = do
  -- Render each new show
  mapM_ (renderShowCard backend) allShows

  -- Render sentinel for next page or end indicator
  if hasMore
    then renderSentinel [i|/#{nextPageUrl}|] "#shows-list"
    else renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ showsLinks.list (Just (currentPage + 1)) (fmap (Filter . Just) maybeTagId) (fmap (Filter . Just) maybeStatus) (fmap (Filter . Just) maybeSearch) (fmap (Filter . Just) maybeSortBy)
