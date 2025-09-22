module API.User.Login.Get where

--------------------------------------------------------------------------------

import API.User.Login.Form (template)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Observability qualified as Observability
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/login"
    ( "user"
        :> "login"
        :> Servant.Header "HX-Current-Url" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.QueryParam "redirect" Text
        :> Servant.QueryParam "email" EmailAddress
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Trace.Tracer ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m (Lucid.Html ())
handler _tracer hxCurrentUrl hxRequest redirectQueryParam emailQueryParam = do
  let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
      isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False
  if isHtmxRequest
    then loadContentOnly loginForm
    else loadFrame loginForm
