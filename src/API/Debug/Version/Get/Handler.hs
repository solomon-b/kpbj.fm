module API.Debug.Version.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Data.Version (showVersion)
import Paths_kpbj_api (version)

--------------------------------------------------------------------------------

handler ::
  AppM String
handler = pure $ showVersion version
