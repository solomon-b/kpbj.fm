module API.User.Current where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.User (User (..))
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as SAS

--------------------------------------------------------------------------------

handler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => SAS.AuthResult User -> m User
handler (SAS.Authenticated user) = pure user
handler _ = Servant.throwError Servant.err401
