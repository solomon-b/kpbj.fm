{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get.Handler (handler, action, UsersListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Users.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardUsersLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the users list page.
data UsersListViewData = UsersListViewData
  { ulvUserMetadata :: UserMetadata.Model,
    ulvSidebarShows :: [Shows.Model],
    ulvSelectedShow :: Maybe Shows.Model,
    ulvViewerId :: User.Id,
    ulvViewerRole :: UserMetadata.UserRole,
    ulvUsers :: [UserMetadata.UserWithMetadata],
    ulvPage :: Int64,
    ulvHasMore :: Bool,
    ulvQueryFilter :: Maybe Text,
    ulvRoleFilter :: Maybe UserMetadata.UserRole,
    ulvSortBy :: UserSortBy,
    ulvIsAppendRequest :: Bool
  }

-- | Business logic: pagination setup, fetch users.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter UserMetadata.UserRole) ->
  Maybe (Filter UserSortBy) ->
  HxRequest ->
  ExceptT HandlerError AppM UsersListViewData
action user userMetadata maybePage queryFilterParam roleFilterParam sortFilterParam hxRequest = do
  -- 1. Set up pagination and filters
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      roleFilter = getFilter =<< roleFilterParam
      queryFilter = getFilter =<< queryFilterParam
      sortBy = fromMaybe JoinDateNewest (getFilter =<< sortFilterParam)
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- 2. Fetch shows for sidebar
  sidebarShows <-
    lift $
      if UserMetadata.isAdmin userMetadata.mUserRole
        then fromRight [] <$> execQuery Shows.getAllActiveShows
        else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))
  let selectedShow = listToMaybe sidebarShows

  -- 3. Fetch users
  allUsers <- fetchUsers limit offset queryFilter roleFilter sortBy

  -- 4. Compute pagination
  let users = take (fromIntegral limit) allUsers
      hasMore = length allUsers > fromIntegral limit

  pure
    UsersListViewData
      { ulvUserMetadata = userMetadata,
        ulvSidebarShows = sidebarShows,
        ulvSelectedShow = selectedShow,
        ulvViewerId = User.mId user,
        ulvViewerRole = userMetadata.mUserRole,
        ulvUsers = users,
        ulvPage = page,
        ulvHasMore = hasMore,
        ulvQueryFilter = queryFilter,
        ulvRoleFilter = roleFilter,
        ulvSortBy = sortBy,
        ulvIsAppendRequest = isAppendRequest
      }

handler ::
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter UserMetadata.UserRole) ->
  Maybe (Filter UserSortBy) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage queryFilterParam roleFilterParam sortFilterParam cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Users list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage queryFilterParam roleFilterParam sortFilterParam hxRequest
    now <- liftIO getCurrentTime
    if vd.ulvIsAppendRequest
      then
        pure $ renderItemsFragment vd.ulvViewerId vd.ulvViewerRole now vd.ulvUsers vd.ulvPage vd.ulvHasMore vd.ulvQueryFilter vd.ulvRoleFilter vd.ulvSortBy
      else do
        let usersTemplate = template vd.ulvViewerId vd.ulvViewerRole now vd.ulvUsers vd.ulvPage vd.ulvHasMore vd.ulvQueryFilter vd.ulvRoleFilter vd.ulvSortBy
            filtersContent = Just $ filtersUI vd.ulvQueryFilter vd.ulvRoleFilter vd.ulvSortBy
        lift $ renderDashboardTemplate hxRequest vd.ulvUserMetadata vd.ulvSidebarShows vd.ulvSelectedShow NavUsers filtersContent Nothing usersTemplate

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
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "w-32"]
        ]
      -- Role filter
      Lucid.select_
        [ Lucid.name_ "role",
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary]
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
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary]
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
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold, "hover:opacity-80"]
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
  ExceptT HandlerError AppM [UserMetadata.UserWithMetadata]
fetchUsers limit offset maybeQuery (maybeToList -> roles) sortBy =
  fromRightM throwDatabaseError $
    lift $ case maybeQuery of
      Just query ->
        execQuery (UserMetadata.searchUsers query roles (limit + 1) offset sortBy)
      Nothing ->
        execQuery (UserMetadata.getUsersByRole roles (limit + 1) offset sortBy)
