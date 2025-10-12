{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log.Class qualified as Log
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

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

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => HxRequest -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate hxRequest mUserInfo templateContent =
  case (mUserInfo, hxRequest) of
    (Just userInfo, IsNotHxRequest) ->
      loadFrameWithUser userInfo templateContent
    (_, IsHxRequest) ->
      loadContentOnly templateContent
    (_, IsNotHxRequest) ->
      loadFrame templateContent
