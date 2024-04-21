module API.User.Current where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Domain.Types.User (User)
import Errors (throw401')
import Log qualified
import Servant.Auth.Server qualified as SAS

--------------------------------------------------------------------------------

handler :: (Log.MonadLog m, MonadThrow m) => SAS.AuthResult User -> m User
handler (SAS.Authenticated user) = pure user
handler _ = throw401'
