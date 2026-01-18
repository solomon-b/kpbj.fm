module API.Static.Get.Handler where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Monad (AppM)
import Servant qualified

--------------------------------------------------------------------------------

handler :: Environment -> Servant.ServerT Servant.Raw AppM
handler = \case
  -- TODO: Safe Links here?
  Production -> Servant.serveDirectoryWebApp "/static"
  Development -> Servant.serveDirectoryWebApp "./static"
