{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new show table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.Shows.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Templates.Page (renderShowRow)
import API.Links (dashboardShowsLinks)
import API.Types
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Filter (Filter (..))
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the show rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  [Shows.ShowWithHostInfo] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe Shows.Status ->
  Lucid.Html ()
renderItemsFragment theShows currentPage hasMore maybeQuery maybeStatusFilter = do
  -- Render each new show row
  mapM_ renderShowRow theShows

  -- Render sentinel for next page or end indicator
  -- For tables, we use a tr element as sentinel
  if hasMore
    then
      Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
        Lucid.td_ [Lucid.colspan_ "5"] $
          renderSentinel [i|/#{nextPageUrl}|] "#shows-table-body"
    else
      Lucid.tr_ [] $
        Lucid.td_ [Lucid.colspan_ "5"] $
          renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter))
