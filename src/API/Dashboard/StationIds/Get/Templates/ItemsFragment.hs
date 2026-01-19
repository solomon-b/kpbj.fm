{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new station ID table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.StationIds.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.Get.Templates.Page (renderStationIdRow)
import API.Links (dashboardStationIdsLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.String.Interpolate (i)
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the station ID rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  [StationIds.StationIdWithCreator] ->
  PageNumber ->
  Bool ->
  Lucid.Html ()
renderItemsFragment stationIds (PageNumber pageNum) hasMore =
  renderTableFragment
    5 -- Number of columns
    "#station-ids-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ renderStationIdRow stationIds)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardStationIdsLinks.list (Just (PageNumber (pageNum + 1)))
