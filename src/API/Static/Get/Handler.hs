module API.Static.Get.Handler where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Servant qualified

--------------------------------------------------------------------------------

handler :: (MonadReader env m, Has Environment env) => Environment -> Servant.ServerT Servant.Raw m
handler = \case
  -- TODO: Safe Links here?
  Production -> Servant.serveDirectoryWebApp "/static"
  Development -> Servant.serveDirectoryWebApp "./static"
