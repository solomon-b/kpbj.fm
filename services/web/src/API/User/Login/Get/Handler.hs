module API.User.Login.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Login.Get.Templates.Form (template)
import App.Common (renderUnauthTemplate)
import App.Monad (AppM)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Text ->
  Maybe HxRequest ->
  Maybe Text ->
  Maybe EmailAddress ->
  AppM (Lucid.Html ())
handler hxCurrentUrl hxRequest redirectQueryParam emailQueryParam = do
  let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
  renderUnauthTemplate (foldHxReq hxRequest) loginForm
