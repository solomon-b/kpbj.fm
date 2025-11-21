{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Users.Get where

--------------------------------------------------------------------------------

import API.Admin.Users.Get.Templates.Error (errorTemplate, notAuthorizedTemplate, notLoggedInTemplate)
import API.Admin.Users.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /admin/users"
    ( "admin"
        :> "users"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "q" Text
        :> Servant.QueryParam "role" UserMetadata.UserRole
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
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage maybeQuery maybeRoleFilter cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_user, userMetadata) ->
      if not (UserMetadata.isAdmin userMetadata.mUserRole)
        then renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
        else do
          let page = fromMaybe 1 maybePage
              limit = 20
              offset = (page - 1) * limit

          getUsersResults limit offset maybeQuery maybeRoleFilter >>= \case
            Left _err -> do
              Log.logInfo "Failed to fetch users from database" ()
              renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to load users. Please try again.")
            Right allUsers -> do
              let users = take (fromIntegral limit) allUsers
                  hasMore = length allUsers > fromIntegral limit
                  usersTemplate = template users page hasMore maybeQuery maybeRoleFilter
              renderTemplate hxRequest (Just userMetadata) usersTemplate

getUsersResults ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Int64 ->
  Int64 ->
  Maybe Text ->
  Maybe UserMetadata.UserRole ->
  m (Either HSQL.Pool.UsageError [UserMetadata.UserWithMetadata])
getUsersResults limit offset maybeQuery maybeRoleFilter = do
  case (maybeQuery, maybeRoleFilter) of
    (Just query, _) ->
      -- Search takes precedence over role filter
      execQuerySpan (UserMetadata.searchUsers query (limit + 1) offset)
    (Nothing, Just role) ->
      -- Filter by role
      execQuerySpan (UserMetadata.getUsersByRole role (limit + 1) offset)
    (Nothing, Nothing) ->
      -- No filters, get all users
      execQuerySpan (UserMetadata.getAllUsersWithPagination (limit + 1) offset)
