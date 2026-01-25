{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Users.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardUsersLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter UserMetadata.UserRole) ->
  Maybe (Filter UserSortBy) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer maybePage queryFilterParam roleFilterParam sortFilterParam cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Users list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination and filters
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        roleFilter = getFilter =<< roleFilterParam
        queryFilter = getFilter =<< queryFilterParam
        sortBy = fromMaybe JoinDateNewest (getFilter =<< sortFilterParam)
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 4. Fetch users
    allUsers <- fetchUsers limit offset queryFilter roleFilter sortBy

    -- 5. Render response
    now <- liftIO getCurrentTime
    let users = take (fromIntegral limit) allUsers
        hasMore = length allUsers > fromIntegral limit

    let viewerRole = userMetadata.mUserRole

    if isAppendRequest
      then
        pure $ renderItemsFragment viewerRole now users page hasMore queryFilter roleFilter sortBy
      else do
        let usersTemplate = template viewerRole now users page hasMore queryFilter roleFilter sortBy
            filtersContent = Just $ filtersUI queryFilter roleFilter sortBy
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers filtersContent Nothing usersTemplate

-- | Filter UI for top bar
filtersUI :: Maybe Text -> Maybe UserMetadata.UserRole -> UserSortBy -> Lucid.Html ()
filtersUI queryFilter roleFilter sortBy = do
  let dashboardUsersGetUrl = Links.linkURI $ dashboardUsersLinks.list Nothing Nothing Nothing Nothing
  Lucid.form_
    [ hxGet_ [i|/#{dashboardUsersGetUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "flex items-center gap-3"
    ]
    $ do
      -- Search input
      Lucid.input_
        [ Lucid.type_ "search",
          Lucid.name_ "q",
          Lucid.value_ (fromMaybe "" queryFilter),
          Lucid.placeholder_ "Search...",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 dark:border-gray-600 w-32"
        ]
      -- Role filter
      Lucid.select_
        [ Lucid.name_ "role",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 dark:border-gray-600"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> selectedWhen (isNothing roleFilter)) "All Roles"
          Lucid.option_ ([Lucid.value_ "User"] <> selectedWhen (roleFilter == Just UserMetadata.User)) "User"
          Lucid.option_ ([Lucid.value_ "Host"] <> selectedWhen (roleFilter == Just UserMetadata.Host)) "Host"
          Lucid.option_ ([Lucid.value_ "Staff"] <> selectedWhen (roleFilter == Just UserMetadata.Staff)) "Staff"
          Lucid.option_ ([Lucid.value_ "Admin"] <> selectedWhen (roleFilter == Just UserMetadata.Admin)) "Admin"
      -- Sort filter
      Lucid.select_
        [ Lucid.name_ "sort",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 dark:border-gray-600"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ "newest"] <> selectedWhen (sortBy == JoinDateNewest)) "Newest"
          Lucid.option_ ([Lucid.value_ "oldest"] <> selectedWhen (sortBy == JoinDateOldest)) "Oldest"
          Lucid.option_ ([Lucid.value_ "name"] <> selectedWhen (sortBy == NameAZ)) "Name A-Z"
          Lucid.option_ ([Lucid.value_ "shows"] <> selectedWhen (sortBy == ShowCount)) "Show Count"
          Lucid.option_ ([Lucid.value_ "status"] <> selectedWhen (sortBy == StatusSuspended)) "Status"
      -- Submit button
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "px-4 py-2 text-sm bg-gray-800 text-white font-bold hover:bg-gray-700"
        ]
        "Filter"
  where
    selectedWhen cond = [Lucid.selected_ "selected" | cond]

fetchUsers ::
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  UserSortBy ->
  AppM [UserMetadata.UserWithMetadata]
fetchUsers limit offset maybeQuery (maybeToList -> roles) sortBy = do
  result <- case maybeQuery of
    Just query ->
      execQuerySpan (UserMetadata.searchUsers query roles (limit + 1) offset sortBy)
    Nothing ->
      execQuerySpan (UserMetadata.getUsersByRole roles (limit + 1) offset sortBy)
  case result of
    Left err -> throwDatabaseError err
    Right users -> pure users
