module API.User.VerifyEmailSent.Get.Handler where

--------------------------------------------------------------------------------

import API.User.VerifyEmailSent.Get.Templates.Page qualified as Page
import App.Common (renderUnauthTemplate)
import App.Monad (AppM)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.HxRequest (HxRequest (..))
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Text ->
  Maybe EmailAddress ->
  AppM (Lucid.Html ())
handler hxRequest mEmail = do
  let hxReq = if hxRequest == Just "true" then IsHxRequest else IsNotHxRequest
      content = Page.template mEmail
  renderUnauthTemplate hxReq content
