module API.User.Login.Get where

--------------------------------------------------------------------------------

import API.User.Login.Form (template)
-- import Component.Frame (loadFrame)

import App.Observability qualified as Observability
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.Header "HX-Current-Url" Text
    :> Servant.QueryParam "redirect" Text
    :> Servant.QueryParam "email" EmailAddress
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m (Lucid.Html ())
handler hxCurrentUrl redirectQueryParam emailQueryParam =
  Observability.handlerSpan "GET /user/login" $ do
    pure $ template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
