{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardUsersLinks, userLinks)
import API.Types (DashboardUsersRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
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
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter UserMetadata.UserRole) ->
  Maybe (Filter UserSortBy) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage queryFilterParam roleFilterParam sortFilterParam cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      roleFilter = getFilter =<< roleFilterParam
      queryFilter = getFilter =<< queryFilterParam
      sortBy = fromMaybe JoinDateNewest (getFilter =<< sortFilterParam)

  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to access this page."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = fromRight [] showsResult
          selectedShow = listToMaybe allShows

      getUsersResults limit offset queryFilter roleFilter sortBy >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch users from database" ()
          let banner = BannerParams Error "Error" "Failed to load users. Please try again."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
        Right allUsers -> do
          now <- liftIO getCurrentTime
          let users = take (fromIntegral limit) allUsers
              hasMore = length allUsers > fromIntegral limit
              usersTemplate = template now users page hasMore queryFilter roleFilter sortBy
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
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 w-32"
        ]
      -- Role filter
      Lucid.select_
        [ Lucid.name_ "role",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300"
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
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300"
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
          Lucid.class_ "px-3 py-1 text-sm bg-gray-800 text-white font-bold hover:bg-gray-700"
        ]
        "Filter"
  where
    selectedWhen cond = [Lucid.selected_ "selected" | cond]

getUsersResults ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  UserSortBy ->
  m (Either HSQL.Pool.UsageError [UserMetadata.UserWithMetadata])
getUsersResults limit offset maybeQuery (maybeToList -> roles) sortBy =
  case maybeQuery of
    Just query ->
      execQuerySpan (UserMetadata.searchUsers query roles (limit + 1) offset sortBy)
    Nothing ->
      execQuerySpan (UserMetadata.getUsersByRole roles (limit + 1) offset sortBy)
