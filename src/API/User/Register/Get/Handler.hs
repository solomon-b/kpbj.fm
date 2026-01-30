module API.User.Register.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Register.Get.Templates.Form (template)
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Text ->
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  AppM (Lucid.Html ())
handler hxRequest emailAddress displayName fullName = do
  mGoogleAnalyticsId <- asks getter
  let registerForm = template displayName fullName emailAddress Nothing
      isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False
  if isHtmxRequest
    then loadContentOnly registerForm
    else loadFrame mGoogleAnalyticsId registerForm
