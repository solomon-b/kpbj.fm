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

import API.Dashboard.Shows.Get.Templates.Page (IsAdmin, renderShowRow)
import API.Links (dashboardShowsLinks)
import API.Types
import Component.Table (renderTableFragment)
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
  IsAdmin ->
  [Shows.ShowWithHostInfo] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe Shows.Status ->
  Lucid.Html ()
renderItemsFragment isAdmin theShows currentPage hasMore maybeQuery maybeStatusFilter =
  renderTableFragment
    4 -- Number of columns (was incorrectly 5 before)
    "#shows-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ (renderShowRow isAdmin) theShows)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter))
