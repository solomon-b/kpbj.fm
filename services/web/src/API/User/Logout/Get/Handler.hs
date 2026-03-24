{-# LANGUAGE ViewPatterns #-}

module API.User.Logout.Get.Handler where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Monad (AppM)
import Component.Flash (clearFlashCookie, jsRedirectBody)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Has qualified as Has
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.Cookie (Cookie (..))
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler (coerce -> cookie) = do
  env <- asks (Has.getter @Environment)
  -- Try to expire the session if we have a valid cookie
  case cookie >>= Auth.lookupSessionId env of
    Nothing -> pure ()
    Just sessionId -> void $ Auth.expireServerSession sessionId
  let expireCookie = Auth.mkCookieSessionExpired env (Domains.cookieDomainMaybe env)
  throwM $
    Servant.ServerError
      { errHTTPCode = 200,
        errReasonPhrase = "OK",
        errBody = Lucid.renderBS $ jsRedirectBody "/",
        errHeaders =
          [ ("Content-Type", "text/html; charset=utf-8"),
            ("HX-Redirect", "/"),
            ("Set-Cookie", Text.Encoding.encodeUtf8 expireCookie),
            ("Set-Cookie", Text.Encoding.encodeUtf8 clearFlashCookie)
          ]
      }
