module API.Media.Get where

--------------------------------------------------------------------------------

import Control.Monad.Reader (MonadReader)
import Servant qualified

--------------------------------------------------------------------------------

handler :: (MonadReader env m) => Servant.ServerT Servant.Raw m
handler = Servant.serveDirectoryWebApp "/tmp/kpbj"
