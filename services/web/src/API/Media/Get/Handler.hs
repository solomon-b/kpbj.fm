module API.Media.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Servant qualified

--------------------------------------------------------------------------------

handler :: Servant.ServerT Servant.Raw AppM
handler = Servant.serveDirectoryWebApp "/tmp/kpbj"
