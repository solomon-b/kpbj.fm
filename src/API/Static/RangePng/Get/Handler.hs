module API.Static.RangePng.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Assets.Embedded qualified as Assets
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------

handler :: AppM ByteString
handler = pure Assets.rangePng
