module API.Static.Get where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "static" :> Servant.Raw

handler :: (MonadReader env m, Has Environment env) => Environment -> Servant.ServerT Servant.Raw m
handler = \case
  -- TODO: Safe Links here?
  Production -> Servant.serveDirectoryWebApp "/backend/static"
  Development -> Servant.serveDirectoryWebApp "./backend/static"
