{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Get (Route, handler) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (rootGetLink, userLoginGetLink)
import API.Dashboard.Users.Get.Templates.Page (template)
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
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
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI rootGetLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/users"
    ( "dashboard"
        :> "users"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "q" (Filter Text)
        :> Servant.QueryParam "role" (Filter UserMetadata.UserRole)
        :> Servant.QueryParam "sort" (Filter UserSortBy)
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
      let allShows = either (const []) id showsResult
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
          renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers usersTemplate

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
