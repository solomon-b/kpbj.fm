{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new ephemeral upload table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.EphemeralUploads.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.Page (renderEphemeralUploadRow)
import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.String.Interpolate (i)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the ephemeral upload rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  StorageBackend ->
  -- | Whether the current user is staff or admin (can edit)
  Bool ->
  [EphemeralUploads.EphemeralUploadWithCreator] ->
  PageNumber ->
  Bool ->
  Lucid.Html ()
renderItemsFragment backend isStaffOrAdmin ephemeralUploads (PageNumber pageNum) hasMore =
  renderTableFragment
    5 -- Number of columns
    "#ephemeral-uploads-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ (renderEphemeralUploadRow backend isStaffOrAdmin) ephemeralUploads)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list (Just (PageNumber (pageNum + 1)))
