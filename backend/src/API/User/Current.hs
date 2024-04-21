module API.User.Current where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Domain.Types.User (User)
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as SAS

--------------------------------------------------------------------------------

handler :: (Log.MonadLog m, MonadThrow m) => SAS.AuthResult User -> m User
handler (SAS.Authenticated user) = pure user
handler _ = throwM Servant.err401
