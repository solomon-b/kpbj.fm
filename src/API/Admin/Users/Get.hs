{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Users.Get (Route, handler) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (rootGetLink, userLoginGetLink)
import API.Admin.Users.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
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
    "GET /admin/users"
    ( "admin"
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
      renderTemplate hxRequest Nothing (redirectWithBanner [i|/#{userLoginGetUrl}|] banner)
    Just (_user, userMetadata) ->
      if not (UserMetadata.isAdmin userMetadata.mUserRole) || isSuspended userMetadata
        then do
          let banner = BannerParams Error "Admin Access Required" "You do not have permission to access this page."
          renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{rootGetUrl}|] banner)
        else do
          getUsersResults limit offset queryFilter roleFilter sortBy >>= \case
            Left _err -> do
              Log.logInfo "Failed to fetch users from database" ()
              let banner = BannerParams Error "Error" "Failed to load users. Please try again."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{rootGetUrl}|] banner)
            Right allUsers -> do
              let users = take (fromIntegral limit) allUsers
                  hasMore = length allUsers > fromIntegral limit
                  usersTemplate = template users page hasMore queryFilter roleFilter sortBy
              renderTemplate hxRequest (Just userMetadata) usersTemplate

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
