{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (dashboardUsersLinks)
import API.Types
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, toGregorian, utctDay)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

-- | Users list template (filters are now in the top bar)
template ::
  UTCTime ->
  [UserMetadata.UserWithMetadata] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  UserSortBy ->
  Lucid.Html ()
template now users currentPage hasMore maybeQuery maybeRoleFilter sortBy = do
  -- User table or empty state
  if null users
    then renderEmptyState maybeQuery
    else do
      Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8, Tokens.fullWidth]] $
        Lucid.table_ [Lucid.class_ $ cls [Tokens.fullWidth]] $ do
          Lucid.thead_ [Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite]] $
            Lucid.tr_ $ do
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-left"]] "Display Name"
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-left"]] "Email"
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-left"]] "Role"
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-left"]] "Status"
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-left"]] "Member Since"
              Lucid.th_ [Lucid.class_ $ cls [Tokens.p4, "text-center", "w-24"]] ""
          Lucid.tbody_ $
            mapM_ (renderUserRow now) users

      renderPagination currentPage hasMore maybeQuery maybeRoleFilter sortBy

renderUserRow :: UTCTime -> UserMetadata.UserWithMetadata -> Lucid.Html ()
renderUserRow now user =
  let userId = user.uwmUserId
      displayName = user.uwmDisplayName
      email = user.uwmEmail
      userRole = user.uwmUserRole
      createdAt = user.uwmUserCreatedAt
      suspendedAt = user.uwmSuspendedAt
      userDetailUrl = Links.linkURI $ dashboardUsersLinks.detail userId
      cellLinkAttrs =
        [ Lucid.class_ $ cls [Tokens.p4, "cursor-pointer"],
          hxGet_ [i|/#{userDetailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
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
   in do
        Lucid.tr_
          [ Lucid.id_ rowId,
            Lucid.class_ $ cls ["border-b-2", "border-gray-200", "hover:bg-gray-50"]
          ]
          $ do
            Lucid.td_ cellLinkAttrs $
              Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold]] $
                Lucid.toHtml (display displayName)

            Lucid.td_ cellLinkAttrs $
              Lucid.toHtml (display email)

            Lucid.td_ cellLinkAttrs $
              renderRoleBadge userRole

            Lucid.td_ cellLinkAttrs $
              renderStatusBadge suspendedAt

            Lucid.td_ cellLinkAttrs $ do
              Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm]] $ Lucid.toHtml (formatMonthYear createdAt)
              Lucid.div_ [Lucid.class_ $ cls ["text-xs", Tokens.textGray600]] $ Lucid.toHtml (formatRelativeTime now createdAt)

            Lucid.td_ [Lucid.class_ $ cls [Tokens.p4, "text-center"]]
              $ Lucid.select_
                [ Lucid.class_ $ cls ["p-2", "border", "border-gray-400", "text-xs", Tokens.bgWhite],
                  xData_ "{}",
                  xOnChange_
                    [i|
                    const action = $el.value;
                    $el.value = '';
                    if (action === 'suspend') {
                      if (confirm('#{suspendConfirmMessage}')) {
                        htmx.ajax('POST', '/#{userSuspendUrl}', {target: '\##{rowId}', swap: 'outerHTML', values: {reason: 'Suspended by admin'}});
                      }
                    } else if (action === 'unsuspend') {
                      if (confirm('#{unsuspendConfirmMessage}')) {
                        htmx.ajax('POST', '/#{userUnsuspendUrl}', {target: '\##{rowId}', swap: 'outerHTML'});
                      }
                    } else if (action === 'delete') {
                      if (confirm('#{deleteConfirmMessage}')) {
                        htmx.ajax('DELETE', '/#{userDeleteUrl}', {target: '\##{rowId}', swap: 'outerHTML'});
                      }
                    }
                  |],
                  xOnClick_ "event.stopPropagation()"
                ]
              $ do
                Lucid.option_ [Lucid.value_ ""] "Actions..."
                if isSuspended
                  then Lucid.option_ [Lucid.value_ "unsuspend"] "Unsuspend"
                  else Lucid.option_ [Lucid.value_ "suspend"] "Suspend"
                Lucid.option_ [Lucid.value_ "delete"] "Delete"

renderRoleBadge :: UserMetadata.UserRole -> Lucid.Html ()
renderRoleBadge role = do
  let (bgClass, textClass, roleText) = case role of
        UserMetadata.Admin -> ("bg-red-100", "text-red-800", "Admin") :: (Text, Text, Text)
        UserMetadata.Staff -> ("bg-purple-100", "text-purple-800", "Staff")
        UserMetadata.Host -> ("bg-blue-100", "text-blue-800", "Host")
        UserMetadata.User -> ("bg-gray-100", "text-gray-800", "User")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml roleText

renderStatusBadge :: Maybe UTCTime -> Lucid.Html ()
renderStatusBadge = \case
  Nothing ->
    Lucid.span_
      [Lucid.class_ "inline-block px-3 py-1 text-sm font-bold rounded bg-green-100 text-green-800"]
      "Active"
  Just _ ->
    Lucid.span_
      [Lucid.class_ "inline-block px-3 py-1 text-sm font-bold rounded bg-yellow-100 text-yellow-800"]
      "Suspended"

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState maybeQuery = do
  Lucid.div_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-12 text-center"] $ do
    Lucid.p_ [Lucid.class_ "text-xl text-gray-600"] $
      case maybeQuery of
        Nothing -> "No users found."
        Just query -> Lucid.toHtml $ "No users found matching \"" <> query <> "\"."

renderPagination :: Int64 -> Bool -> Maybe Text -> Maybe UserMetadata.UserRole -> UserSortBy -> Lucid.Html ()
renderPagination currentPage hasMore (Just . Filter -> maybeQuery) (Just . Filter -> maybeRoleFilter) sortBy = do
  Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
    -- Previous button
    if currentPage > 1
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{prevPageUrl}|],
            hxGet_ [i|/#{prevPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "<- PREVIOUS"
      else
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [Lucid.class_ "text-gray-600 font-bold"] $
      Lucid.toHtml $
        "Page " <> show currentPage

    -- Next button
    if hasMore
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{nextPageUrl}|],
            hxGet_ [i|/#{nextPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "NEXT ->"
      else
        Lucid.div_ [] mempty
  where
    maybeSortFilter = if sortBy == JoinDateNewest then Nothing else Just (Filter (Just sortBy))
    prevPageUrl = Links.linkURI $ dashboardUsersLinks.list (Just (currentPage - 1)) maybeQuery maybeRoleFilter maybeSortFilter
    nextPageUrl = Links.linkURI $ dashboardUsersLinks.list (Just (currentPage + 1)) maybeQuery maybeRoleFilter maybeSortFilter

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
