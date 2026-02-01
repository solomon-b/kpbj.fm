{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get.Templates.Page
  ( template,
    renderUserRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUsersLinks)
import API.Types
import Component.ActionsDropdown qualified as ActionsDropdown
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, toGregorian, utctDay)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

-- | Users list template (filters are now in the top bar)
template ::
  -- | Viewer's user ID (to prevent self-deletion)
  User.Id ->
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
template viewerId viewerRole now users currentPage hasMore maybeQuery maybeRoleFilter sortBy = do
  -- User table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null users
      then renderEmptyState maybeQuery
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "users-table-body",
              itcHeaders =
                [ ColumnHeader "Display Name" AlignLeft,
                  ColumnHeader "Email" AlignLeft,
                  ColumnHeader "Role" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Member Since" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ (renderUserRow viewerId viewerRole now) users)
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
    prevPageUrl :: Links.URI
    prevPageUrl =
      Links.linkURI $
        dashboardUsersLinks.list
          (Just (currentPage - 1))
          (Just . Filter $ maybeQuery)
          (Just . Filter $ maybeRoleFilter)
          maybeSortFilter

renderUserRow ::
  -- | Viewer's user ID (to prevent self-deletion)
  User.Id ->
  -- | Viewer's role (for permission checks)
  UserMetadata.UserRole ->
  UTCTime ->
  UserMetadata.UserWithMetadata ->
  Lucid.Html ()
renderUserRow viewerId viewerRole now user =
  let userId = user.uwmUserId
      displayName = user.uwmDisplayName
      email = user.uwmEmail
      userRole = user.uwmUserRole
      createdAt = user.uwmUserCreatedAt
      suspendedAt = user.uwmSuspendedAt
      userDetailUri = Links.linkURI $ dashboardUsersLinks.detail userId
      userDetailUrl = [i|/#{userDetailUri}|]
      userEditUrl = Links.linkURI $ dashboardUsersLinks.editGet userId
      userDeleteUrl = Links.linkURI $ dashboardUsersLinks.delete userId
      userSuspendUrl = Links.linkURI $ dashboardUsersLinks.suspendPost userId
      userUnsuspendUrl = Links.linkURI $ dashboardUsersLinks.unsuspendPost userId
      userIdText = display userId
      rowId = [i|user-row-#{userIdText}|]
      deleteConfirmMessage =
        "Are you sure you want to delete user \""
          <> display displayName
          <> "\" ("
          <> display email
          <> ")? This action cannot be undone."
      suspendConfirmMessage =
        "Are you sure you want to suspend user \""
          <> display displayName
          <> "\"? They will see a warning banner and cannot perform host actions."
      unsuspendConfirmMessage =
        "Are you sure you want to unsuspend user \""
          <> display displayName
          <> "\"? They will regain full access to their account."
      isSuspended = isJust suspendedAt
      rowTarget = "#" <> rowId
      suspendAction =
        if isSuspended
          then
            [ ActionsDropdown.htmxPostAction
                "unsuspend"
                "Unsuspend"
                [i|/#{userUnsuspendUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just unsuspendConfirmMessage)
                []
            ]
          else
            [ ActionsDropdown.htmxPostAction
                "suspend"
                "Suspend"
                [i|/#{userSuspendUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just suspendConfirmMessage)
                [("reason", "Suspended by admin")]
            ]
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          Lucid.td_ (clickableCellAttrs userDetailUrl) $
            Lucid.span_ [class_ $ base [Tokens.fontBold]] $
              Lucid.toHtml (display displayName)

          Lucid.td_ (clickableCellAttrs userDetailUrl) $
            Lucid.toHtml (display email)

          Lucid.td_ (clickableCellAttrs userDetailUrl) $
            renderRoleBadge userRole

          Lucid.td_ (clickableCellAttrs userDetailUrl) $
            renderStatusBadge suspendedAt

          Lucid.td_ (clickableCellAttrs userDetailUrl) $ do
            Lucid.div_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml (formatMonthYear createdAt)
            Lucid.div_ [class_ $ base ["text-xs", Tokens.fgMuted]] $ Lucid.toHtml (formatRelativeTime now createdAt)

          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            ActionsDropdown.render $
              [ ActionsDropdown.navigateAction
                  "edit"
                  "Edit"
                  [i|/#{userEditUrl}|]
              ]
                <> suspendAction
                <> [ ActionsDropdown.htmxDeleteAction
                       "delete"
                       "Delete"
                       [i|/#{userDeleteUrl}|]
                       rowTarget
                       ActionsDropdown.SwapOuterHTML
                       deleteConfirmMessage
                     | viewerRole == UserMetadata.Admin && viewerId /= userId
                   ]

renderRoleBadge :: UserMetadata.UserRole -> Lucid.Html ()
renderRoleBadge role = do
  let (bgClass, textClass, roleText) = case role of
        UserMetadata.Admin -> (Tokens.errorBg, Tokens.errorText, "Admin") :: (Text, Text, Text)
        UserMetadata.Staff -> (Tokens.warningBg, Tokens.warningText, "Staff")
        UserMetadata.Host -> (Tokens.infoBg, Tokens.infoText, "Host")
        UserMetadata.User -> (Tokens.bgAlt, Tokens.fgPrimary, "User")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml roleText

renderStatusBadge :: Maybe UTCTime -> Lucid.Html ()
renderStatusBadge = \case
  Nothing ->
    Lucid.span_
      [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{Tokens.successBg} #{Tokens.successText}|]]
      "Active"
  Just _ ->
    Lucid.span_
      [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{Tokens.warningBg} #{Tokens.warningText}|]]
      "Suspended"

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState maybeQuery = do
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.fgMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [Lucid.class_ "text-xl"] $
      case maybeQuery of
        Nothing -> "No users found."
        Just query -> Lucid.toHtml $ "No users found matching \"" <> query <> "\"."

formatMonthYear :: UTCTime -> String
formatMonthYear = formatTime defaultTimeLocale "%B %Y"

formatRelativeTime :: UTCTime -> UTCTime -> String
formatRelativeTime now time =
  let (year, month, _) = toGregorian (utctDay time)
      (currentYear, currentMonth, _) = toGregorian (utctDay now)
      totalMonths = (currentYear - year) * 12 + fromIntegral (currentMonth - month)
      years = totalMonths `div` 12
      months = totalMonths `mod` 12
   in case (years, months) of
        (0, 0) -> "This month"
        (0, 1) -> "1 month"
        (0, m) -> show m <> " months"
        (1, 0) -> "1 year"
        (y, 0) -> show y <> " years"
        (1, m) -> "1 year, " <> show m <> " months"
        (y, m) -> show y <> " years, " <> show m <> " months"
