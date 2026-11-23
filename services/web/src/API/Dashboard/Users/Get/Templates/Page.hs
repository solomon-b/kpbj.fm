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
import Component.Badge qualified as Badge
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Control.Monad (when)
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
import Lucid.HTMX
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

  -- Stable swap target for the one-time "reset password" modal. Rendered once
  -- outside the table so it is unaffected by infinite-scroll row fragments; the
  -- admin action swaps the modal fragment into it.
  Lucid.div_ [Lucid.id_ "reset-password-modal"] mempty
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
      userResetPasswordUrl = Links.linkURI $ dashboardUsersLinks.resetPasswordPost userId
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
      resetPasswordConfirmMessage =
        "Generate a new password for \""
          <> display displayName
          <> "\" and sign them out of all sessions? You'll see the new password once."
      isSuspended = isJust suspendedAt
      rowTarget = "#" <> rowId
      suspendButton =
        if isSuspended
          then
            Button.render
              ( Button.defaults
                  & Button.with
                    [ hxPost_ [i|/#{userUnsuspendUrl}|],
                      hxTarget_ rowTarget,
                      hxSwap_ "outerHTML",
                      hxConfirm_ unsuspendConfirmMessage
                    ]
              )
              "Unsuspend"
          else
            Button.render
              ( Button.defaults
                  & Button.with
                    [ hxPost_ [i|/#{userSuspendUrl}|],
                      hxTarget_ rowTarget,
                      hxSwap_ "outerHTML",
                      hxConfirm_ suspendConfirmMessage,
                      hxVals_ "{\"reason\": \"Suspended by admin\"}"
                    ]
              )
              "Suspend"
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

          Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
            Button.group $ do
              Button.render (Button.defaults & Button.dashboardLink [i|/#{userEditUrl}|]) "Edit"
              suspendButton
              when (viewerRole == UserMetadata.Admin) $
                Button.render
                  ( Button.defaults
                      & Button.with
                        [ hxPost_ [i|/#{userResetPasswordUrl}|],
                          hxTarget_ "#reset-password-modal",
                          hxSwap_ "innerHTML",
                          hxConfirm_ resetPasswordConfirmMessage
                        ]
                  )
                  "Reset Password"
              when (viewerRole == UserMetadata.Admin && viewerId /= userId) $
                Button.render
                  ( Button.defaults
                      & Button.danger
                      & Button.with
                        [ hxDelete_ [i|/#{userDeleteUrl}|],
                          hxTarget_ rowTarget,
                          hxSwap_ "outerHTML",
                          hxConfirm_ deleteConfirmMessage
                        ]
                  )
                  "Delete"

renderRoleBadge :: UserMetadata.UserRole -> Lucid.Html ()
renderRoleBadge = \case
  UserMetadata.Admin -> Badge.badge Badge.Regular Badge.Error "Admin"
  UserMetadata.Staff -> Badge.badge Badge.Regular Badge.Warning "Staff"
  UserMetadata.Host -> Badge.badge Badge.Regular Badge.Info "Host"
  UserMetadata.User -> Badge.badge Badge.Regular Badge.Plain "User"

renderStatusBadge :: Maybe UTCTime -> Lucid.Html ()
renderStatusBadge = \case
  Nothing -> Badge.badge Badge.Regular Badge.Success "Active"
  Just _ -> Badge.badge Badge.Regular Badge.Warning "Suspended"

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
