{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Get.Templates.Page where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminUserDeleteLink, adminUserDetailGetLink, adminUsersGetLink)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

template ::
  [UserMetadata.UserWithMetadata] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  Lucid.Html ()
template users currentPage hasMore maybeQuery maybeRoleFilter = do
  -- Success/Error banner container (for HTMX out-of-band swaps)
  Lucid.div_ [Lucid.id_ "success-banner-container"] mempty

  -- Page header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold"] "USER MANAGEMENT"
    Lucid.p_ [Lucid.class_ "text-gray-600 mt-2"] "Manage all users, roles, and permissions"

  -- Filters section
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.form_
      [ hxGet_ [i|/admin/users|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-4"
      ]
      $ do
        -- Search input
        Lucid.div_ $ do
          Lucid.label_ [Lucid.for_ "search", Lucid.class_ "block font-bold mb-2"] "Search"
          Lucid.input_
            [ Lucid.type_ "search",
              Lucid.name_ "q",
              Lucid.id_ "search",
              Lucid.value_ (fromMaybe "" maybeQuery),
              Lucid.placeholder_ "Search by name or email...",
              Lucid.class_ "w-full p-3 border-2 border-gray-800"
            ]

        -- Role filter
        Lucid.div_ $ do
          Lucid.label_ [Lucid.for_ "role", Lucid.class_ "block font-bold mb-2"] "Filter by Role"
          Lucid.select_
            [ Lucid.name_ "role",
              Lucid.id_ "role",
              Lucid.class_ "w-full p-3 border-2 border-gray-800"
            ]
            $ do
              Lucid.option_ [Lucid.value_ "", selectedIf (isNothing maybeRoleFilter)] "All Roles"
              Lucid.option_ [Lucid.value_ "User", selectedIf (maybeRoleFilter == Just UserMetadata.User)] "User"
              Lucid.option_ [Lucid.value_ "Host", selectedIf (maybeRoleFilter == Just UserMetadata.Host)] "Host"
              Lucid.option_ [Lucid.value_ "Staff", selectedIf (maybeRoleFilter == Just UserMetadata.Staff)] "Staff"
              Lucid.option_ [Lucid.value_ "Admin", selectedIf (maybeRoleFilter == Just UserMetadata.Admin)] "Admin"

        -- Submit button (for non-JS support)
        Lucid.div_ [Lucid.class_ "md:col-span-2 flex gap-4"] $ do
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
            ]
            "FILTER"
          when (isJust maybeQuery || isJust maybeRoleFilter) $
            Lucid.a_
              [ Lucid.href_ "/admin/users",
                hxGet_ "/admin/users",
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-300 text-gray-800 px-6 py-3 font-bold hover:bg-gray-400"
              ]
              "CLEAR FILTERS"

  -- User table or empty state
  if null users
    then renderEmptyState maybeQuery
    else do
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 overflow-hidden mb-8 w-full"] $
        Lucid.table_ [Lucid.class_ "w-full"] $ do
          Lucid.thead_ [Lucid.class_ "bg-gray-800 text-white"] $
            Lucid.tr_ $ do
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Display Name"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Email"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Role"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Joined"
              Lucid.th_ [Lucid.class_ "p-4 text-right"] "Actions"
          Lucid.tbody_ $
            mapM_ renderUserRow users

      renderPagination currentPage hasMore maybeQuery maybeRoleFilter
  where
    selectedIf condition = if condition then Lucid.selected_ "selected" else mempty
    when cond action = if cond then action else mempty

renderUserRow :: UserMetadata.UserWithMetadata -> Lucid.Html ()
renderUserRow user =
  let userId = user.uwmUserId
      displayName = user.uwmDisplayName
      email = user.uwmEmail
      userRole = user.uwmUserRole
      createdAt = user.uwmUserCreatedAt
      userDetailUrl = Links.linkURI $ adminUserDetailGetLink userId
      userDeleteUrl = Links.linkURI $ adminUserDeleteLink userId
      userIdText = display userId
      rowId = [i|user-row-#{userIdText}|]
      confirmMessage =
        "Are you sure you want to delete user \""
          <> display displayName
          <> "\" ("
          <> display email
          <> ")? This action cannot be undone."
   in do
        Lucid.tr_
          [ Lucid.id_ rowId,
            Lucid.class_ "border-b-2 border-gray-200 hover:bg-gray-50"
          ]
          $ do
            Lucid.td_ [Lucid.class_ "p-4"] $
              Lucid.span_ [Lucid.class_ "font-bold"] $
                Lucid.toHtml (display displayName)

            Lucid.td_ [Lucid.class_ "p-4"] $
              Lucid.toHtml (display email)

            Lucid.td_ [Lucid.class_ "p-4"] $
              renderRoleBadge userRole

            Lucid.td_ [Lucid.class_ "p-4"] $
              Lucid.toHtml (formatDate createdAt)

            Lucid.td_ [Lucid.class_ "p-4 text-right"] $ do
              Lucid.a_
                [ Lucid.href_ [i|/#{userDetailUrl}|],
                  hxGet_ [i|/#{userDetailUrl}|],
                  hxTarget_ "#main-content",
                  hxPushUrl_ "true",
                  Lucid.class_ "text-blue-600 font-bold hover:underline mr-4"
                ]
                "View"

              Lucid.button_
                [ hxDelete_ [i|/#{userDeleteUrl}|],
                  hxTarget_ [i|\##{rowId}|],
                  hxSwap_ "outerHTML",
                  hxConfirm_ confirmMessage,
                  Lucid.class_ "text-red-600 font-bold hover:underline"
                ]
                "Delete"

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

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState maybeQuery = do
  Lucid.div_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-12 text-center"] $ do
    Lucid.p_ [Lucid.class_ "text-xl text-gray-600"] $
      case maybeQuery of
        Nothing -> "No users found."
        Just query -> Lucid.toHtml $ "No users found matching \"" <> query <> "\"."

renderPagination :: Int64 -> Bool -> Maybe Text -> Maybe UserMetadata.UserRole -> Lucid.Html ()
renderPagination currentPage hasMore maybeQuery maybeRoleFilter = do
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
          "← PREVIOUS"
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
          "NEXT →"
      else
        Lucid.div_ [] mempty
  where
    prevPageUrl = Links.linkURI $ adminUsersGetLink (Just (currentPage - 1)) maybeQuery maybeRoleFilter
    nextPageUrl = Links.linkURI $ adminUsersGetLink (Just (currentPage + 1)) maybeQuery maybeRoleFilter

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"
