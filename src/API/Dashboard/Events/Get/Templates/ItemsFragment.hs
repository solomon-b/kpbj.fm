{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new event table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.Events.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Templates.Page (renderEventRow)
import API.Links (dashboardEventsLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the event rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  [Events.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
renderItemsFragment events currentPage hasMore =
  renderTableFragment
    5 -- Number of columns
    "#events-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ renderEventRow events)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEventsLinks.list (Just (currentPage + 1))
