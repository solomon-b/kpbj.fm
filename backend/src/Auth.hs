{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Display (Display)
import Data.Text.Encoding qualified as Text.Encoding
import Deriving.Aeson qualified as Deriving
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User (User)
import Effects.Database.Queries.User
import Effects.Database.Utils
import Errors (throw500')
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Log qualified
import Servant.Auth.JWT (ToJWT)
import Servant.Auth.Server qualified
import Servant.Auth.Server qualified as Servant.Auth

--------------------------------------------------------------------------------
-- User Auth

type instance Servant.Auth.BasicAuthCfg = Servant.Auth.BasicAuthData -> IO (Servant.Auth.AuthResult User)

instance Servant.Auth.FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

-- TODO: Hash password!
checkAuth :: HSQL.Pool -> Log.Logger -> Servant.Auth.BasicAuthData -> IO (Servant.Auth.AuthResult User)
checkAuth pool logger (Servant.Auth.BasicAuthData email' pass') =
  let email = EmailAddress $ CI.mk $ Text.Encoding.decodeUtf8 email'
      pass = Password $ Text.Encoding.decodeUtf8 pass'
   in HSQL.use pool (HSQL.statement () (selectUserByCredentialQuery email pass)) >>= \case
        Left err -> do
          Log.runLogT "kpbj-backend" logger Log.defaultLogLevel $
            Log.logAttention "SQL Error" (show err)
          pure Servant.Auth.Indefinite
        Right (Just (parseModel -> user)) ->
          pure $ Servant.Auth.Authenticated user
        Right Nothing ->
          pure Servant.Auth.NoSuchUser

--------------------------------------------------------------------------------

newtype JWTToken = JWTToken {getJWTToken :: Text}
  deriving stock (Generic)
  deriving newtype (Display)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "getJWT", Deriving.CamelToSnake]] JWTToken

--------------------------------------------------------------------------------

generateJWTToken ::
  ( MonadReader env m,
    Has Servant.Auth.Server.JWTSettings env,
    Log.MonadLog m,
    MonadIO m,
    ToJWT a,
    MonadThrow m
  ) =>
  a ->
  m JWTToken
generateJWTToken a = do
  jwtSettings <- Reader.asks Has.getter
  liftIO (Servant.Auth.Server.makeJWT a jwtSettings Nothing) >>= \case
    Left _err -> throw500'
    Right jwt ->
      pure $ JWTToken $ Text.Encoding.decodeUtf8 $ BL.toStrict jwt
