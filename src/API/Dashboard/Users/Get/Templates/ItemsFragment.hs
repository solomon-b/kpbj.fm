{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new user table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.Users.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.Page (renderUserRow)
import API.Links (dashboardUsersLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types.Filter (Filter (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the user rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  -- | Viewer's role (for permission checks)
  UserMetadata.UserRole ->
  UTCTime ->
  [UserMetadata.UserWithMetadata] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  UserSortBy ->
  Lucid.Html ()
renderItemsFragment viewerRole now users currentPage hasMore maybeQuery maybeRoleFilter sortBy =
  renderTableFragment
    6 -- Number of columns
    "#users-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ (renderUserRow viewerRole now) users)
  where
    maybeSortFilter = if sortBy == JoinDateNewest then Nothing else Just (Filter (Just sortBy))
    nextPageUrl :: Links.URI
    nextPageUrl =
      Links.linkURI $
        dashboardUsersLinks.list
          (Just (currentPage + 1))
          (Just . Filter $ maybeQuery)
          (Just . Filter $ maybeRoleFilter)
          maybeSortFilter
