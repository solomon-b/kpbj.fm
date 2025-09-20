module API where

--------------------------------------------------------------------------------

import API.User.Login.Get qualified as User.Login.Get
import App qualified
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Log (MonadLog)
import Lucid.Base qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

runApi :: IO ()
runApi = App.runApp @API server ()

--------------------------------------------------------------------------------

type API = User.Login.Get.Route

--------------------------------------------------------------------------------

server ::
  ( Has Tracer env,
    MonadCatch m,
    MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  p ->
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m (Lucid.Base.Html ())
server _ = User.Login.Get.handler

--------------------------------------------------------------------------------

-- | Route: GET /user/login
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userLoginGetLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Get.Route)
