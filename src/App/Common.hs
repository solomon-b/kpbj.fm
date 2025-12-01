{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.DashboardFrame (DashboardNav)
import Component.DashboardFrame qualified as DashboardFrame
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log.Class qualified as Log
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------
-- Authorization

data AuthorizationCheck = Authorized | Unauthorized
  deriving stock (Show, Eq)

-- | Check if a user has admin privileges
checkAdminAuthorization ::
  Maybe (User.Model, UserMetadata.Model) ->
  AuthorizationCheck
checkAdminAuthorization = \case
  Nothing ->
    Unauthorized
  Just (_user, userMeta) ->
    bool Unauthorized Authorized (UserMetadata.isAdmin userMeta.mUserRole)

--------------------------------------------------------------------------------

-- | Get user info
getUserInfo ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Maybe Cookie ->
  m (Maybe (User.Model, UserMetadata.Model))
getUserInfo (coerce -> cookie) =
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      pure Nothing
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right userMetadata ->
          pure ((user,) <$> userMetadata)
        _ -> do
          Log.logAttention "Failed to query user_metadata" (Aeson.object ["user.id" .= user.mId])
          pure Nothing

-- | Render template with proper HTMX handling and suspension status
--
-- Shows a warning banner at the top of every page for suspended users.
-- Pass NotSuspended for unauthenticated users or when suspension status is unknown.
renderTemplate :: (Log.MonadLog m, MonadCatch m) => HxRequest -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate hxRequest mUserInfo templateContent =
  case (mUserInfo, hxRequest) of
    (Just userInfo, IsNotHxRequest) ->
      loadFrameWithUser userInfo templateContent
    (_, IsHxRequest) ->
      loadContentOnly templateContent
    (_, IsNotHxRequest) ->
      loadFrame templateContent

-- | Render dashboard template with separate full-page layout
--
-- The dashboard uses its own frame with a left sidebar navigation,
-- completely separate from the main site's header/navigation.
renderDashboardTemplate ::
  (Log.MonadLog m, MonadCatch m) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Lucid.Html () ->
  m (Lucid.Html ())
renderDashboardTemplate hxRequest userInfo allShows selectedShow activeNav templateContent =
  case hxRequest of
    IsHxRequest ->
      DashboardFrame.loadDashboardContentOnly templateContent
    IsNotHxRequest ->
      DashboardFrame.loadDashboardFrame userInfo allShows selectedShow activeNav templateContent
