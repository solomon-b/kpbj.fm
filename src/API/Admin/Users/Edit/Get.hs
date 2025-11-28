{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Users.Edit.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminUsersGetLink, rootGetLink, userLoginGetLink)
import API.Admin.Users.Edit.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
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

adminUsersGetUrl :: Links.URI
adminUsersGetUrl = Links.linkURI $ adminUsersGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /admin/users/:id/edit"
    ( "admin"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "edit"
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
  User.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer targetUserId cookie (foldHxReq -> hxRequest) = do
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
          -- Fetch target user and their metadata
          userDataResult <- execTransactionSpan $ do
            maybeTargetUser <- HT.statement () (User.getUser targetUserId)
            maybeTargetMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
            pure (maybeTargetUser, maybeTargetMetadata)

          case userDataResult of
            Left _err -> do
              Log.logInfo "Failed to fetch user from database" ()
              let banner = BannerParams Error "Error" "Failed to load user. Please try again."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{adminUsersGetUrl}|] banner)
            Right (Nothing, _) -> do
              let banner = BannerParams Warning "User Not Found" "The user you are trying to edit does not exist."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{adminUsersGetUrl}|] banner)
            Right (_, Nothing) -> do
              let banner = BannerParams Warning "User Not Found" "The user you are trying to edit does not exist."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{adminUsersGetUrl}|] banner)
            Right (Just targetUser, Just targetMetadata) ->
              renderTemplate hxRequest (Just userMetadata) (template targetUser targetMetadata)
