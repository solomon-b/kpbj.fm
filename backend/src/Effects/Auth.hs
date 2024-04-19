module Effects.Auth where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Deriving.Aeson qualified as Deriving
import GHC.Generics (Generic)
import Log qualified
import Servant qualified
import Servant.Auth.JWT
import Servant.Auth.Server qualified

--------------------------------------------------------------------------------
-- Domain

newtype JWTToken = JWTToken {getJWTToken :: Text}
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "getJWT", Deriving.CamelToSnake]] JWTToken

--------------------------------------------------------------------------------

generateJWTToken ::
  ( MonadReader env m,
    Has Servant.Auth.Server.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m,
    ToJWT a
  ) =>
  a ->
  m JWTToken
generateJWTToken a = do
  jwtSettings <- Reader.asks Has.getter
  liftIO (Servant.Auth.Server.makeJWT a jwtSettings Nothing) >>= \case
    Left _err -> Servant.throwError Servant.err500
    Right jwt ->
      pure $ JWTToken $ Text.Encoding.decodeUtf8 $ BL.toStrict jwt
