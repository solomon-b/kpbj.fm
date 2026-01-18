module API.User.Login.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Login.Get.Templates.Form (template)
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace

--------------------------------------------------------------------------------

handler ::
  Trace.Tracer ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  AppM (Lucid.Html ())
handler _tracer hxCurrentUrl hxRequest redirectQueryParam emailQueryParam = do
  mGoogleAnalyticsId <- asks getter
  let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
      isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False
  if isHtmxRequest
    then loadContentOnly loginForm
    else loadFrame mGoogleAnalyticsId loginForm
