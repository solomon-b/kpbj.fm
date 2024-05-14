module API.User.Register where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Text.Display (Display, display)
import Data.Text.Display.Generic (RecordInstance (..))
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class (MonadDB)
import Database.Queries.User
import Database.Utils
import Deriving.Aeson qualified as Deriving
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User
import Errors (throw401, throw500')
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant.Auth.Server qualified as SAS
import Text.Email.Validate qualified as Email
import Tracing (handlerSpan)

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password,
    urDisplayName :: DisplayName
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Register)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

handler ::
  ( MonadReader env m,
    Has SAS.JWTSettings env,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Register ->
  m Auth.JWTToken
handler req@Register {..} = do
  handlerSpan "/user/register" req display $ do
    unless (Email.isValid $ Text.Encoding.encodeUtf8 $ CI.original $ coerce urEmail) $ throw401 "Invalid Email Address"
    execQuerySpanThrowMessage "Failed to query users table" (selectUserByEmailQuery urEmail) >>= \case
      Just _ -> do
        Log.logInfo "Email address is already registered" urEmail
        throw401 "Email address is already registered"
      Nothing -> do
        Log.logInfo "Registering New User" urEmail
        uid <- insertUser (urEmail, urPassword, urDisplayName, IsNotAdmin)
        execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid) >>= \case
          Nothing ->
            throw500'
          Just user ->
            Auth.generateJWTToken $ parseModel @_ @User user
