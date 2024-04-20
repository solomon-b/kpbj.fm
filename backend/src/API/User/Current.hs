module API.User.Current where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError)
import Domain.Types.User (User)
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as SAS

--------------------------------------------------------------------------------

handler :: (MonadError Servant.ServerError m, Log.MonadLog m) => SAS.AuthResult User -> m User
handler (SAS.Authenticated user) = pure user
handler _ = Servant.throwError Servant.err401
