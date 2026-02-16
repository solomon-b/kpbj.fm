{-# LANGUAGE OverloadedRecordDot #-}
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
import API.Shows.Get.Templates.Page (ShowsViewData (..))
import API.Types
import Component.Card.Show (renderShowCard)
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.String.Interpolate (i)
import Domain.Types.Filter (Filter (..))
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the show items and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing items container.
renderItemsFragment :: ShowsViewData -> Lucid.Html ()
renderItemsFragment vd = do
  let backend = vd.svStorageBackend
      allShows = vd.svShows
      hasMore = vd.svHasMore
  -- Render each new show
  mapM_ (renderShowCard backend) allShows

  -- Render sentinel for next page or end indicator
  if hasMore
    then renderSentinel [i|/#{nextPageUrl}|] "#shows-list"
    else renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ showsLinks.list (Just (vd.svCurrentPage + 1)) (fmap (Filter . Just) vd.svTagFilter) (fmap (Filter . Just) vd.svStatusFilter) (fmap (Filter . Just) vd.svSearchFilter) (fmap (Filter . Just) vd.svSortByFilter)
