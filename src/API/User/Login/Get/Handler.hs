module API.User.Login.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Login.Get.Templates.Form (template)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Has (Has, getter)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m,
    Has (Maybe GoogleAnalyticsId) env
  ) =>
  Trace.Tracer ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m (Lucid.Html ())
handler _tracer hxCurrentUrl hxRequest redirectQueryParam emailQueryParam = do
  mGoogleAnalyticsId <- asks getter
  let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
      isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False
  if isHtmxRequest
    then loadContentOnly loginForm
    else loadFrame mGoogleAnalyticsId loginForm
