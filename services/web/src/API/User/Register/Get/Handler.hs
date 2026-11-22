module API.User.Register.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Register.Get.Templates.Form (template)
import App.Common (renderUnauthTemplate)
import App.Monad (AppM)
import Component.Flash (FlashMessage)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.HxRequest (HxRequest (..))
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe FlashMessage ->
  Maybe Text ->
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  AppM (Lucid.Html ())
handler mFlash hxRequest emailAddress displayName fullName = do
  let registerForm = template displayName fullName emailAddress Nothing
      hxReq = case hxRequest of
        Just "true" -> IsHxRequest
        _ -> IsNotHxRequest
  renderUnauthTemplate hxReq mFlash registerForm
