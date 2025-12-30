{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new episode table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.Episodes.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import API.Links (dashboardEpisodesLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the episode rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  UserMetadata.Model ->
  Shows.Model ->
  [Episodes.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
renderItemsFragment userMeta showModel episodes currentPage hasMore =
  renderTableFragment
    4 -- Number of columns
    "#episodes-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ (renderEpisodeTableRow userMeta showModel) episodes)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage + 1))
