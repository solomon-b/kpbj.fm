module Effects.Database.Class where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Hasql.Pool (UsageError)
import Hasql.Session (Session)
import Hasql.Statement (Statement)

--------------------------------------------------------------------------------

class (MonadIO m) => MonadDB m where
  runDB :: Session a -> m (Either UsageError a)
  execStatement :: Statement () a -> m (Either UsageError a)
