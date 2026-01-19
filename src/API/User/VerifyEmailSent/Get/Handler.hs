module API.User.VerifyEmailSent.Get.Handler where

--------------------------------------------------------------------------------

import API.User.VerifyEmailSent.Get.Templates.Page qualified as Page
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
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
  Maybe EmailAddress ->
  AppM (Lucid.Html ())
handler _tracer hxRequest mEmail = do
  mGoogleAnalyticsId <- asks getter
  let isHtmxRequest = hxRequest == Just "true"
      content = Page.template mEmail
  if isHtmxRequest
    then loadContentOnly content
    else loadFrame mGoogleAnalyticsId content
