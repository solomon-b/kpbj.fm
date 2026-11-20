module API.User.Login.Get.Handler where

--------------------------------------------------------------------------------

import API.User.Login.Get.Templates.Form (template)
import App.Common (getUserInfo, renderUnauthTemplate)
import App.Monad (AppM)
import Component.Flash (jsRedirectBody)
import Control.Applicative ((<|>))
import Control.Monad.Catch (throwM)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe Text ->
  Maybe HxRequest ->
  Maybe Text ->
  Maybe EmailAddress ->
  AppM (Lucid.Html ())
handler cookie hxCurrentUrl hxRequest redirectQueryParam emailQueryParam = do
  mUserInfo <- getUserInfo cookie
  case mUserInfo of
    Just _ -> do
      let isRelative u = Text.isPrefixOf "/" u && not (Text.isPrefixOf "//" u)
          target = case redirectQueryParam of
            Just u | isRelative u -> u
            _ -> "/"
      throwM $
        Servant.ServerError
          { Servant.errHTTPCode = 200,
            Servant.errReasonPhrase = "OK",
            Servant.errBody = Lucid.renderBS $ jsRedirectBody target,
            Servant.errHeaders =
              [ ("Content-Type", "text/html; charset=utf-8"),
                ("HX-Redirect", Text.encodeUtf8 target)
              ]
          }
    Nothing -> do
      let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
      renderUnauthTemplate (foldHxReq hxRequest) loginForm
