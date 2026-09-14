module API.Debug.Version.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Data.Version (showVersion)
import Paths_kpbj_web (version)

--------------------------------------------------------------------------------

handler ::
  AppM String
handler = pure $ showVersion version
