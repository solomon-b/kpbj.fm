module API.User.Register.Get where

--------------------------------------------------------------------------------

import API.User.Register.Get.Templates.Form (template)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
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
    MonadReader env m
  ) =>
  Trace.Tracer ->
  Maybe Text ->
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  m (Lucid.Html ())
handler _tracer hxRequest emailAddress displayName fullName = do
  let registerForm = template displayName fullName emailAddress Nothing
      isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False
  if isHtmxRequest
    then loadContentOnly registerForm
    else loadFrame registerForm
